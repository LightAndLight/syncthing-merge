{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Toml
  ( load
  , TomlError (..)
  , Sage.ParseError (..)
  , Located (..)
  , Decoder
  , key
  , tableArray
  , ValueDecoder
  , string
  , text

    -- * TOML syntax
  , parse
  , Toml (..)
  , TomlKeyEntry (..)
  )
where

import Control.Applicative (many, some, (<|>))
import Control.Monad (unless)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT, get, lift, put, runStateT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import Data.Either (partitionEithers)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Sage as Sage

load :: FilePath -> Decoder a -> IO (Either TomlError a)
load path decoder = do
  contents <- ByteString.readFile path
  case parse contents of
    Left err -> pure $ Left err
    Right toml ->
      case decode toml decoder of
        Left err -> pure $ Left err
        Right a -> pure $ Right a

data TomlError
  = ParseError
      !Sage.ParseError
  | MissingKey
      -- | Offset
      !Int
      -- | Key name
      !Text
  | UnexpectedEntries ![TomlKeyEntry] ![Located TomlItem]
  | ExpectedString
      -- | Offset
      !Int
  deriving (Show)

parse :: ByteString -> Either TomlError Toml
parse input =
  case Sage.parse (tomlParser <* Sage.eof) input of
    Left err -> Left $ ParseError err
    Right a -> pure a
  where
    located ma = Located <$> Sage.getOffset <*> ma

    token ma = ma <* Sage.skipSome (Sage.satisfy (`elem` " \t"))

    newlines = Sage.skipSome (void (Sage.char '\n') <|> void (Sage.string $ fromString "\r\n"))

    sepEndBy ma sep =
      (:) <$> ma <*> loop
        <|> pure []
      where
        loop =
          sep *> ((:) <$> ma <*> loop <|> pure [])
            <|> pure []

    sepBy1 ma sep =
      (:) <$> ma <*> (sep *> Sage.sepBy ma sep <|> pure [])

    tomlParser :: Sage.Parser Toml
    tomlParser =
      Toml
        <$> located (Map.fromList <$> sepEndBy keyParser newlines)
        <*> many (located itemParser)

    nameParser :: Sage.Parser Text
    nameParser = fmap Text.pack (some . Sage.satisfy $ (||) <$> Char.isAlphaNum <*> (`elem` "_-"))

    keyParser :: Sage.Parser (Text, TomlKeyEntry)
    keyParser =
      (\(Located keyOffset name) -> (,) name . TomlKeyEntry keyOffset)
        <$> located (token nameParser)
        <* token (Sage.char '=')
        <*> located valueParser

    quoted = "\\\""

    valueParser :: Sage.Parser TomlValue
    valueParser =
      VString . Text.pack
        <$ Sage.char '"'
        <*> many (Sage.satisfy (`notElem` quoted) <|> Sage.char '\\' *> Sage.satisfy (`elem` quoted))
        <* Sage.char '"'

    itemParser :: Sage.Parser TomlItem
    itemParser =
      tableArrayParser

    tableArrayParser =
      TomlTableArray
        <$ Sage.string (fromString "[[")
        <*> sepBy1 nameParser (Sage.char '.')
        <* Sage.string (fromString "]]")
        <* newlines
        <*> fmap Map.fromList (sepEndBy keyParser newlines)

data Located a = Located {locatedOffset :: !Int, locatedValue :: !a}
  deriving (Show)

data Toml
  = Toml
      !(Located (Map Text TomlKeyEntry))
      [Located TomlItem]
  deriving (Show)

data TomlKeyEntry
  = TomlKeyEntry
      -- | Offset of key name
      !Int
      -- | Value
      !(Located TomlValue)
  deriving (Show)

data TomlItem
  = TomlTableArray ![Text] !(Map Text TomlKeyEntry)
  deriving (Show)

data TomlValue
  = VString !Text
  | VInt !Int
  deriving (Show)

newtype Decoder a = Decoder (StateT Toml (Either TomlError) a)
  deriving (Functor, Applicative)

decode :: Toml -> Decoder a -> Either TomlError a
decode toml (Decoder decoder) = do
  (a, Toml (Located _offset keys) entries) <- runStateT decoder toml
  unless (Map.null keys && null entries) . throwError $
    UnexpectedEntries (Map.elems keys) entries
  pure a

key :: Text -> ValueDecoder a -> Decoder a
key name valueDecoder = Decoder $ do
  Toml (Located offset keys) entries <- get
  case Map.lookup name keys of
    Just (TomlKeyEntry _keyOffset value') -> do
      a <- lift $ valueDecode value' valueDecoder
      let keys' = Map.delete name keys
      put $ Toml (Located offset keys') entries
      pure a
    Nothing ->
      throwError $ MissingKey offset name

tableArray :: Text -> Decoder a -> Decoder [a]
tableArray name (Decoder decoder) = Decoder $ do
  Toml keys entries <- get

  let
    matchingOrNonMatching =
      foldr
        ( \(Located offset entry) rest ->
            case entry of
              TomlTableArray (part : parts) entries'
                | part == name ->
                    Left (Located offset $ TomlTableArray parts entries') : rest
              _ ->
                Right (Located offset entry) : rest
        )
        []
        entries

    (matching, nonMatching) = partitionEithers matchingOrNonMatching

  let
    loop [] =
      pure []
    loop (Located offset (TomlTableArray [] entries') : rest) = do
      let (prefix, suffix) = break (\case (Located _offset (TomlTableArray [] _)) -> True; _ -> False) rest
      case runStateT decoder $ Toml (Located offset entries') prefix of
        Left err -> Left err
        Right (a, Toml (Located _offset keys'') entries'') -> do
          unless (Map.null keys'' && null entries'') . throwError $
            UnexpectedEntries (Map.elems keys'') entries''
          (a :) <$> loop suffix
    loop entry@(Located _ (TomlTableArray (_ : _) _) : _) = do
      error $ "impossible: " ++ show entry

  as <- lift $ loop matching

  put $ Toml keys nonMatching
  pure as

newtype ValueDecoder a = ValueDecoder (Located TomlValue -> Either TomlError a)
  deriving (Functor) via (ReaderT (Located TomlValue) (Either TomlError))

valueDecode :: Located TomlValue -> ValueDecoder a -> Either TomlError a
valueDecode value (ValueDecoder decoder) = decoder value

text :: ValueDecoder Text
text =
  ValueDecoder $
    \(Located offset value) ->
      case value of
        VString s -> Right s
        _ -> Left $ ExpectedString offset

string :: ValueDecoder String
string = Text.unpack <$> text
