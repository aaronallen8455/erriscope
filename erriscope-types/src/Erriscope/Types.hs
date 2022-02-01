{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Erriscope.Types
  ( Payload(..)
  , mockPayload
  , FileErrors(..)
  , ErrorMsg(..)
  , ErrorType(..)
  , Location(..)
  , encodePayload
  , decodePayload
  ) where

import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Word
import           Safe

data Payload =
  MkPayload
    { version :: Int
      -- ^ Allows an incompatibility between the plugin and server to be detected
    , fileErrors :: [FileErrors]
    }

mockPayload :: Payload
mockPayload =
  MkPayload
    { version = 0
    , fileErrors =
      [ MkFileErrors
        { filename = "TestFile1"
        , filepath = "src/TestFile1.hs"
        , errors =
          [ MkErrorMsg
            { body = "Error body"
            , errorType = Error
            , fileLocation =
              MkLocation
                { lineNum = 1
                , colNum = 1
                }
            }
          , MkErrorMsg
            { body = "Error body 2"
            , errorType = Error
            , fileLocation =
              MkLocation
                { lineNum = 2
                , colNum = 2
                }
            }
          ]
        }
      ]
    }

encodePayload :: Payload -> BS.ByteString
encodePayload = encode

decodePayload :: BS.ByteString -> Either String Payload
decodePayload = decode

instance Serialize Payload where
  put MkPayload{..} =
    put (fileErrors, version)
  get = do
    (fileErrors, version) <- get
    pure MkPayload{..}

data FileErrors =
  MkFileErrors
    { filename :: BS.ByteString
    , filepath :: BS.ByteString
    , errors :: [ErrorMsg]
    }

instance Serialize FileErrors where
  put MkFileErrors{..} = put (filename, filepath, errors)
  get = do
    (filename, filepath, errors) <- get
    pure MkFileErrors{..}

data ErrorMsg =
  MkErrorMsg
    { body :: BS.ByteString
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
