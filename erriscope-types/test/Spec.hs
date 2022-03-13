import           Hedgehog
import           Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Prelude hiding (FilePath)
import           Test.Hspec
import           Test.Hspec.Hedgehog

import           Erriscope.Types

main :: IO ()
main = hspec $
  describe "Erriscope.Types" $
    it "roundtrips serialization" $
      hedgehog $ do
        env <- forAll envelopeGen
        tripping env encodeEnvelope decodeEnvelope

envelopeGen :: Gen Envelope
envelopeGen =
  MkEnvelope
    <$> int (Range.constant 0 1000000)
    <*> messageGen

messageGen :: Gen Message
messageGen =
  choice
    [ AddError <$> fileErrorGen
    , DeleteFile <$> Gen.maybe filePathGen
    ]

fileErrorGen :: Gen FileError
fileErrorGen =
  MkFileError
    <$> Gen.maybe filePathGen
    <*> Gen.maybe filePathGen
    <*> errorMsgGen

filePathGen :: Gen FilePath
filePathGen = utf8 (Range.constant 1 70) unicodeAll

errorMsgGen :: Gen ErrorMsg
errorMsgGen =
  MkErrorMsg
    <$> utf8 (Range.constant 1 3000) unicodeAll
    <*> Gen.maybe (utf8 (Range.constant 1 100) unicodeAll)
    <*> enumBounded
    <*> Gen.maybe locationGen

errorTypeGen :: Gen ErrorType
errorTypeGen = choice [pure Error, pure Warning]

locationGen :: Gen Location
locationGen =
  MkLocation
    <$> word (Range.constant 1 1000000)
    <*> word (Range.constant 1 1000000)
