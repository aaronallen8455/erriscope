{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Erriscope.Types
  ( Envelope(..)
  , Message(..)
  , FileError(..)
  , ErrorMsg(..)
  , ErrorType(..)
  , ErrorBody
  , Location(..)
  , FilePath
  , ModuleName
  , Port
  , defaultPort
  , validPort
  , getPortFromArgs
  , isWarning
  , encodeEnvelope
  , decodeEnvelope
  ) where

import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.Serialize
import           Data.Word
import           Prelude hiding (FilePath)
import           Safe
import           Text.Read (readMaybe)

type ModuleName = BS.ByteString
type FilePath = BS.ByteString
type ErrorBody = BS.ByteString

data Envelope =
  MkEnvelope
    { version :: Int
      -- ^ Allows an incompatibility between the plugin and server to be detected
    , message :: Message
    } deriving (Eq, Show)

instance Serialize Envelope where
  put MkEnvelope{..} = put (version, message)
  get = do
    (version, message) <- get
    pure MkEnvelope{..}

-- FilePath is Nothing for non-located errors
data Message
  = AddError FileError -- Add an error
  | DeleteFile (Maybe FilePath) -- Remove all existing errors for a file
  deriving (Eq, Show)

instance Serialize Message where
  put (AddError fileError) = do
    put (0 :: Word8)
    put fileError
  put (DeleteFile file) = do
    put (1 :: Word8)
    put file
  get = do
    get @Word8 >>= \case
      0 -> AddError <$> get
      1 -> DeleteFile <$> get
      _ -> fail "Unable to decode Message"

encodeEnvelope :: Envelope -> BS.ByteString
encodeEnvelope = encode

decodeEnvelope :: BS.ByteString -> Either String Envelope
decodeEnvelope = decode

data FileError =
  MkFileError
    { moduleName :: Maybe ModuleName
    , filepath :: Maybe FilePath
    , errorMsg :: ErrorMsg
    } deriving (Show, Eq)

isWarning :: FileError -> Bool
isWarning = (== Warning) . errorType . errorMsg

instance Serialize FileError where
  put MkFileError{..} = put (moduleName, filepath, errorMsg)
  get = do
    (moduleName, filepath, errorMsg) <- get
    pure MkFileError{..}

data ErrorMsg =
  MkErrorMsg
    { body :: ErrorBody
    , caret :: Maybe BS.ByteString
    , errorType :: ErrorType
    , fileLocation :: Maybe Location
    } deriving (Show, Eq)

instance Serialize ErrorMsg where
  put MkErrorMsg{..} =
    put (body, caret, errorType, fileLocation)
  get = do
    (body, caret, errorType, fileLocation) <- get
    pure MkErrorMsg{..}

data ErrorType
  = Error
  | Warning
  deriving (Enum, Bounded, Eq, Show)

instance Serialize ErrorType where
  put = put @Word8 . fromIntegral . fromEnum
  get = maybe (fail "Invalid ErrorType encoding") pure
      . toEnumMay
      . fromIntegral
      =<< get @Word8

data Location =
  MkLocation
    { lineNum :: Word
    , colNum :: Word
    } deriving (Eq, Ord, Show)

instance Serialize Location where
  put MkLocation{..} = put (lineNum, colNum)
  get = do
    (lineNum, colNum) <- get
    pure MkLocation{..}

type Port = Int

defaultPort :: Port
defaultPort = 8888

validPort :: Port -> Bool
validPort port = port >= 1 && port <= 65535

getPortFromArgs :: [String] -> Port
getPortFromArgs args = fromMaybe defaultPort $ do
  firstArg : _ <- pure args
  port <- readMaybe firstArg
  guard $ validPort port
  pure port
