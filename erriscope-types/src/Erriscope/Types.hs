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
  , Location(..)
  , FilePath
  , encodeEnvelope
  , decodeEnvelope
  ) where

import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Word
import           Prelude hiding (FilePath)
import           Safe

type File = BS.ByteString
type FilePath = BS.ByteString
type ErrorBody = BS.ByteString

data Envelope =
  MkEnvelope
    { version :: Int
      -- ^ Allows an incompatibility between the plugin and server to be detected
    , message :: Message
    }

instance Serialize Envelope where
  put MkEnvelope{..} = put (version, message)
  get = do
    (version, message) <- get
    pure MkEnvelope{..}

data Message
  = AddError FileError -- Add an error
  | DeleteFile FilePath -- Remove all existing errors for a file
  | DeleteAll

instance Serialize Message where
  put (AddError fileError) = do
    put (0 :: Word8)
    put fileError
  put (DeleteFile file) = do
    put (1 :: Word8)
    put file
  put DeleteAll = do
    put (2 :: Word8)
  get = do
    get @Word8 >>= \case
      0 -> AddError <$> get
      1 -> DeleteFile <$> get
      2 -> pure DeleteAll
      _ -> fail "Unable to decode Message"

encodeEnvelope :: Envelope -> BS.ByteString
encodeEnvelope = encode

decodeEnvelope :: BS.ByteString -> Either String Envelope
decodeEnvelope = decode

data FileError =
  MkFileError
    { filename :: File
    , filepath :: FilePath
    , errorMsg :: ErrorMsg
    }

instance Serialize FileError where
  put MkFileError{..} = put (filename, filepath, errorMsg)
  get = do
    (filename, filepath, errorMsg) <- get
    pure MkFileError{..}

data ErrorMsg =
  MkErrorMsg
    { body :: ErrorBody
    , errorType :: ErrorType
    , fileLocation :: Location
    }

instance Serialize ErrorMsg where
  put MkErrorMsg{..} =
    put (body, errorType, fileLocation)
  get = do
    (body, errorType, fileLocation) <- get
    pure MkErrorMsg{..}

data ErrorType
  = Error
  | Warning
  deriving (Enum, Bounded)

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
    }

instance Serialize Location where
  put MkLocation{..} = put (lineNum, colNum)
  get = do
    (lineNum, colNum) <- get
    pure MkLocation{..}
