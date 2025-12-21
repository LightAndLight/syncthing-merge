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
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json (Parser)
import Data.ByteString (ByteString)
import Data.Foldable (for_, traverse_)
import Data.List (sortOn, unsnoc)
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Semigroup (Max (..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Client (httpLbs)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Status (ok200)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO (hClose, hGetContents, hPutStrLn, stderr)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)
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
  , syncthingApiKey :: !ByteString
  -- ^ Syncthing API key
  , syncthingFolderId :: !Text
  -- ^ ID of the Syncthing folder in which the @tsk@ database resides
  , syncthingFolder :: !Text
  -- ^ Name of the Syncthing folder in which the @tsk@ database resides
  , syncthingFile :: !Text
  -- ^ Name of the synced file
  , mergeProgram :: !FilePath
  -- ^ Path to the executable responsible for merging
  --
  -- First program argument is the original file, second argument is the conflicting version.
  --
  -- [Syncthing renames the older file in a conflicting pair](https://docs.syncthing.net/users/syncing.html#conflicting-changes).
  -- Therefore, the merge program should use the conflicting file as the base and apply the
  -- changes from the original file on top. After that, the program should clean up by renaming
  -- the updated conflicting file to the original file.
  }

loadConfig :: MonadIO m => m Config
loadConfig =
  Config
    <$> required "SYNCTHING_MERGE_URL" text
    <*> required "SYNCTHING_MERGE_API_KEY" byteString
    <*> required "SYNCTHING_MERGE_FOLDER_ID" text
    <*> required "SYNCTHING_MERGE_FOLDER_NAME" text
    <*> required "SYNCTHING_MERGE_FILE_NAME" text
    <*> required "SYNCTHING_MERGE_PROGRAM" filepath
  where
    required :: MonadIO m => String -> (String -> a) -> m a
    required key decoder = do
      mValue <- liftIO $ lookupEnv key
      case mValue of
        Nothing -> do
          logError $ "missing required environment variable: " ++ key
          liftIO exitFailure
        Just value -> pure $! decoder value

    text = Text.pack
    filepath = id
    byteString = Text.Encoding.encodeUtf8 . Text.pack

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
  }

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
  | event.payload.folder == config.syncthingFolderId =
      -- The conflicting files are all older than the original.
      --
      -- Merge the original file (newest) onto the newest conflicting file. The result of that
      -- is renamed to the original file. Then merge *that* original file onto the second-oldest
      -- conflicting file, and *that* is renamed to the original file. And so on.
      --
      -- It's very likely that only a single conflicting file will be created. But this seems like
      -- the right way to merge multiple conflicts. Essentially a `foldl` back in time.
      for_ (sortOn Down event.payload.filenames) $ \filename ->
        when (filename `conflictsWith` config.syncthingFile) $ do
          let directory = Text.unpack syncthingRoot </> Text.unpack config.syncthingFolder
          let originalFile = directory </> Text.unpack config.syncthingFile
          let conflictFile = directory </> Text.unpack filename
          exists <- liftIO $ doesFileExist conflictFile

          if exists
            then do
              let program = config.mergeProgram
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
                  logInfo $ "merged " ++ originalFile ++ " into " ++ conflictFile
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
  | otherwise =
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

main :: IO ()
main = do
  config <- loadConfig

  manager <- Http.newManager Http.defaultManagerSettings

  paths <- do
    let url = Text.unpack config.syncthingUrl ++ "/rest/system/paths"
    result <-
      getJson @SyncthingPaths manager config.syncthingApiKey url
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

  logInfo "started"
  result <-
    looping initialState $ \continue break state -> do
      let url =
            Text.unpack config.syncthingUrl
              ++ "/rest/events?since="
              ++ show @Int (getMax state.syncthingLatestId)
              ++ "&events=LocalIndexUpdated"
      result <- getJson @[SyncthingEvent LocalIndexUpdated] manager config.syncthingApiKey url
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
