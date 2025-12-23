{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate, tryJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteString.Char8
import Data.Foldable (for_, traverse_)
import Data.List (find, sortOn, unsnoc)
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Semigroup (Max (..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Client (httpLbs)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Status (ok200)
import qualified Options.Applicative as Options
import qualified Secret
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO (hClose, hGetContents, hPutStrLn, stderr, hSetBuffering, BufferMode (..), stdout)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)
import qualified Text.Diagnostic as Diagnostic
import qualified Text.Diagnostic.Sage as Diagnostic.Sage
import qualified Toml
import Prelude hiding (break)

data SyncthingPaths
  = SyncthingPaths
  { auditLog :: !Text
  , baseDir_config :: !Text
  , baseDir_data :: !Text
  , baseDir_userHome :: !Text
  , certFile :: !Text
  , config :: !Text
  , database :: !Text
  , defFolder :: !Text
  , guiAssets :: !Text
  , httpsCertFile :: !Text
  , httpsKeyFile :: !Text
  , keyFile :: !Text
  , lockFile :: !Text
  , logFile :: !Text
  , panicLog :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SyncthingPaths where
  parseJSON =
    Json.genericParseJSON
      Json.defaultOptions{Json.fieldLabelModifier = fmap (\c -> if c == '_' then '-' else c)}

data Config
  = Config
  { syncthingUrl :: !Text
  -- ^ URL (including port, if necessary) at which Syncthing is accessible
  , syncthingApiKeyFile :: !FilePath
  -- ^ File containing Syncthing API key
  , mergeTargets :: [MergeTarget]
  }
  deriving (Show)

data MergeTarget
  = MergeTarget
  { folderId :: !Text
  -- ^ ID of the Syncthing folder in which the @tsk@ database resides
  , folderName :: !Text
  -- ^ Name of the Syncthing folder in which the @tsk@ database resides
  , files :: ![MergeTargetFile]
  }
  deriving (Show)

data MergeTargetFile
  = MergeTargetFile
  { name :: !Text
  -- ^ Name of the synced file
  , program :: !FilePath
  -- ^ Path to the executable responsible for merging
  --
  -- First program argument is the original file, second argument is the conflicting version.
  --
  -- [Syncthing renames the older file in a conflicting pair](https://docs.syncthing.net/users/syncing.html#conflicting-changes).
  -- Therefore, the merge program should use the conflicting file as the base and apply the
  -- changes from the original file on top. After that, the program should clean up by renaming
  -- the updated conflicting file to the original file.
  }
  deriving (Show)

loadConfig :: MonadIO m => FilePath -> m Config
loadConfig path = do
  result <- liftIO $ Toml.load path configDecoder
  case result of
    Right a -> pure $! a
    Left err -> do
      let
        report =
          case err of
            Toml.ParseError parseError -> Diagnostic.Sage.parseError parseError
            Toml.MissingKey offset name ->
              Diagnostic.emit
                (Diagnostic.Offset offset)
                Diagnostic.Caret
                (fromString $ "missing key: " ++ Text.unpack name)
            Toml.UnexpectedEntries keys entries ->
              foldMap
                ( \(Toml.TomlKeyEntry nameOffset _value) ->
                    Diagnostic.emit
                      (Diagnostic.Offset nameOffset)
                      Diagnostic.Caret
                      (fromString "unexpected key")
                )
                keys
                <> foldMap
                  ( \entry ->
                      Diagnostic.emit
                        (Diagnostic.Offset $ Toml.locatedOffset entry)
                        Diagnostic.Caret
                        (fromString "unexpected section")
                  )
                  entries
            Toml.ExpectedString offset ->
              Diagnostic.emit
                (Diagnostic.Offset offset)
                Diagnostic.Caret
                (fromString "unexpected string")

      liftIO $ do
        contents <- LazyByteString.readFile path
        LazyByteString.Char8.putStrLn $
          Diagnostic.render Diagnostic.defaultConfig (fromString path) contents report
        exitFailure

configDecoder :: Toml.Decoder Config
configDecoder =
  Config
    <$> Toml.key (fromString "syncthing-url") Toml.text
    <*> Toml.key (fromString "syncthing-api-key-file") Toml.string
    <*> Toml.tableArray
      (fromString "merge-target")
      ( MergeTarget
          <$> Toml.key (fromString "folder-id") Toml.text
          <*> Toml.key (fromString "folder-name") Toml.text
          <*> Toml.tableArray
            (fromString "file")
            ( MergeTargetFile
                <$> Toml.key (fromString "name") Toml.text
                <*> Toml.key (fromString "program") Toml.string
            )
      )

data SyncthingEvent a
  = SyncthingEvent
  { id :: Int
  , globalID :: Int
  , time :: UTCTime
  , payload :: a
  }
  deriving (Show, Eq, Generic)

class FromSyncthingEvent a where
  parseSyncthingEvent :: Text -> Json.Value -> Json.Parser a

instance FromSyncthingEvent a => FromJSON (SyncthingEvent a) where
  parseJSON = Json.withObject "SyncthingEvent" $ \obj ->
    SyncthingEvent
      <$> obj .: fromString "id"
      <*> obj .: fromString "globalID"
      <*> obj .: fromString "time"
      <*> ( do
              type_ <- obj .: fromString "type"
              data_ <- obj .: fromString "data"
              parseSyncthingEvent type_ data_
          )

data LocalIndexUpdated
  = LocalIndexUpdated
  { filenames :: ![Text]
  , folder :: !Text
  , items :: !Int
  , sequence :: !Int
  , version :: !Int
  } deriving Show

instance FromSyncthingEvent LocalIndexUpdated where
  parseSyncthingEvent type_
    | type_ == fromString "LocalIndexUpdated" =
        Json.withObject "LocalIndexUpdated" $ \obj ->
          LocalIndexUpdated
            <$> obj .: fromString "filenames"
            <*> obj .: fromString "folder"
            <*> obj .: fromString "items"
            <*> obj .: fromString "sequence"
            <*> obj .: fromString "version"
    | otherwise = \_data -> fail $ "invalid type: expected LocalIndexUpdated, got " ++ Text.unpack type_

logInfo, logError :: MonadIO m => String -> m ()
logError = liftIO . hPutStrLn stderr . ("error: " ++)
logInfo = liftIO . hPutStrLn stderr . ("info: " ++)

looping :: Applicative m => s -> (forall r. (s -> m r) -> (a -> m r) -> s -> m r) -> m a
looping s body = loop s
  where
    loop !s' = body loop pure s'

newtype State
  = State
  { syncthingLatestId :: Max Int
  }

instance Semigroup State where
  State a <> State a' = State (a <> a')

instance Monoid State where
  mempty = initialState

initialState :: State
initialState = State{syncthingLatestId = 0}

conflictsWith ::
  -- | New file (e.g. `{filename}.sync-conflict-{date}-{time}-{modifiedBy}.{ext}`)
  Text ->
  -- | Target file (e.g. `{filename}.{ext}`)
  Text ->
  Bool
conflictsWith new target =
  let
    (filename, mExt) =
      case unsnoc $ Text.split (== '.') target of
        Nothing ->
          -- no extension
          (target, Nothing)
        Just (prefix, ext) ->
          (Text.intercalate (fromString ".") prefix, Just ext)
  in
    isJust $ do
      new' <- Text.stripPrefix (filename <> fromString ".") new
      new'' <-
        case mExt of
          Nothing -> pure new'
          Just ext -> Text.stripSuffix (fromString "." <> ext) new'
      Text.stripPrefix (fromString "sync-conflict-") new''

handleConflicts ::
  (MonadWriter State m, MonadIO m) =>
  Config ->
  -- | Syncthing root directory
  Text ->
  SyncthingEvent LocalIndexUpdated ->
  m ()
handleConflicts config syncthingRoot event
  | Just mergeTarget <- find ((event.payload.folder ==) . (.folderId)) config.mergeTargets = do
      -- Merge the original file (newest) onto the newest conflicting file. The result of that
      -- is renamed to the original file. Then merge *that* original file onto the second-oldest
      -- conflicting file, and *that* is renamed to the original file. And so on.
      --
      -- It's very likely that only a single conflicting file will be created. But this seems like
      -- the right way to merge multiple conflicts. Essentially a `foldl` back in time.
      for_ (sortOn Down event.payload.filenames) $ \filename ->
        for_ (find ((filename `conflictsWith`) . (.name)) mergeTarget.files) $ \mergeTargetFile -> do
            let directory = Text.unpack syncthingRoot </> Text.unpack mergeTarget.folderName
            let originalFile = directory </> Text.unpack mergeTargetFile.name
            let conflictFile = directory </> Text.unpack filename
            exists <- liftIO $ doesFileExist conflictFile

            if exists
              then do
                let program = mergeTargetFile.program
                let args = [originalFile, conflictFile]
                let process = (proc program args){std_in = Inherit, std_out = CreatePipe, std_err = CreatePipe}
                (exitCode, out, err) <-
                  liftIO . withCreateProcess process $ \_mStdin mStdout mStderr processHandle ->
                    case (mStdout, mStderr) of
                      (Just hStdout, Just hStderr) -> do
                        out <- hGetContents hStdout
                        err <- hGetContents hStderr

                        outVar <- newEmptyMVar
                        _ <- forkIO $ do
                          evaluate $ rnf out
                          putMVar outVar out
                          hClose hStdout

                        errVar <- newEmptyMVar
                        _ <- forkIO $ do
                          evaluate $ rnf err
                          putMVar errVar err
                          hClose hStderr

                        (,,)
                          <$> waitForProcess processHandle
                          <*> takeMVar outVar
                          <*> takeMVar errVar
                      (Nothing, _) -> error "missing handle for stdout"
                      (_, Nothing) -> error "missing handle for stderr"

                case exitCode of
                  ExitSuccess -> do
                    logInfo . unlines $
                      [ "merged " ++ conflictFile ++ " into " ++ originalFile
                      , "  program: " ++ show program
                      , "  arguments: " ++ show args
                      , "  stdout:"
                      ]
                        ++ fmap ("    " ++) (lines out)
                        ++ ["  stderr:"]
                        ++ fmap ("    " ++) (lines err)
                    tell State{syncthingLatestId = Max event.id}
                  ExitFailure code -> do
                    logError . unlines $
                      [ "failed to merge " ++ conflictFile ++ " into " ++ originalFile
                      , "  program: " ++ show program
                      , "  arguments: " ++ show args
                      , "  exit status: " ++ show code
                      , "  stdout:"
                      ]
                        ++ fmap ("    " ++) (lines out)
                        ++ ["  stderr:"]
                        ++ fmap ("    " ++) (lines err)
              else do
                logInfo $ conflictFile ++ " is missing (assuming already merged)"
                tell State{syncthingLatestId = Max event.id}
  | otherwise = do
      logInfo $ "skipping event with folder " ++ Text.unpack event.payload.folder
      tell State{syncthingLatestId = Max event.id}

data HttpResult a
  = Failure String
  | Timeout
  | Success a

getJson ::
  forall a m.
  (FromJSON a, MonadIO m) =>
  Http.Manager ->
  -- | API key
  ByteString ->
  -- | URL
  String ->
  m (HttpResult a)
getJson manager key url = do
  mResponse <- liftIO . tryJust (\case Http.HttpExceptionRequest _ Http.ResponseTimeout -> Just (); _ -> Nothing) $ do
    request <- Http.applyBearerAuth key <$> Http.parseRequest url
    httpLbs request manager

  case mResponse of
    Left () -> pure Timeout
    Right response ->
      if Http.responseStatus response /= ok200
        then
          pure . Failure . unlines $
            [ "GET " ++ url ++ " failed"
            , "  response body: " ++ show (Http.responseBody response)
            ]
        else do
          let result = Json.eitherDecode @a (Http.responseBody response)
          case result of
            Left err ->
              pure . Failure $ "GET " ++ url ++ " returned unexpected JSON: " ++ err
            Right a ->
              pure $ Success a

newtype Cli
  = Cli
  { configPath :: FilePath
  }

cliParser :: Options.Parser Cli
cliParser =
  Cli
    <$> Options.strOption
      (Options.long "config" <> Options.metavar "PATH" <> Options.help "Path to program settings")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  cli <- Options.execParser $ Options.info cliParser Options.fullDesc
  config <- loadConfig cli.configPath
  syncthingApiKey <-
    Secret.protect . ByteString.Char8.strip <$> ByteString.readFile config.syncthingApiKeyFile

  manager <- Http.newManager Http.defaultManagerSettings

  paths <- do
    let url = Text.unpack config.syncthingUrl ++ "/rest/system/paths"
    result <-
      getJson @SyncthingPaths manager (Secret.leak syncthingApiKey) url
    case result of
      Failure err -> do
        logError err
        exitFailure
      Timeout -> do
        logError $ "timed out on GET " ++ url
        exitFailure
      Success a ->
        pure a

  let syncthingRoot = paths.baseDir_userHome

  logInfo . unlines $
    "watching:" :
    fmap
      ("  " ++)
      [ Text.unpack syncthingRoot </> Text.unpack mergeTarget.folderName </> Text.unpack file.name
      | mergeTarget <- config.mergeTargets, file <- mergeTarget.files
      ]
  result <-
    looping initialState $ \continue break state -> do
      let url =
            Text.unpack config.syncthingUrl
              ++ "/rest/events?since="
              ++ show @Int (getMax state.syncthingLatestId)
              ++ "&events=LocalIndexUpdated"
      result <- getJson @[SyncthingEvent LocalIndexUpdated] manager (Secret.leak syncthingApiKey) url
      case result of
        Failure err -> do
          logError err
          break $ ExitFailure 1
        Timeout -> do
          logInfo $ "timed out on GET " ++ url ++ " (retrying)"
          continue state
        Success events -> do
          state' <- execWriterT (traverse_ (handleConflicts config syncthingRoot) events)
          wait *> continue (state <> state')

  exitWith result
  where
    wait = threadDelay $ 5 * 1_000_000
