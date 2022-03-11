{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.ByteString.Lazy as LBS
import           Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TEL
import           Test.Hspec
import           Test.Hspec.Expectations
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Erriscope.Html as E
import qualified Erriscope.Types as ET

main = hspec $
  describe "Erriscope.Html" $ do
    it "identifies code block correctly" $ do
      let res = fmap fst $ uncurry E.getIndentedPortion
                  =<< E.labeledCodeBlock "xpected type:" servantType
      res `shouldBe` Just servantTypeResult
    it "renders correct HTML" $ do
      let res = renderHtml $ E.renderErrorBody test2
      res `shouldBe` test2Result
    it "identifies code block correctly" $ do
      let res = fmap fst $ uncurry E.getIndentedPortion
                  =<< E.inCodeBlock test3
      res `shouldBe` Just test3Result
    it "handles 'In a stmt of'" $ do
      let res = fmap fst $ uncurry E.getIndentedPortion
                  =<< E.inCodeBlock test4
      res `shouldBe` Just test4Result

servantType :: T.Text
servantType =
  [i|  Expected type: (Vault
                  -> ApplicationRole -> WimsId -> WimsHandler Response)
                 :<|> ((Vault
                        -> AuthTokenOwner -> WimsId -> Request -> WimsHandler Response)
                       :<|> ((Vault
                              -> AuthTokenOwner -> RawJSONText Request -> WimsHandler Response)
                             :<|> ((Vault
                                    -> AuthTokenOwner -> WimsId -> Request -> WimsHandler Response)
                                   :<|> ((Vault
                                          -> AuthTokenOwner
                                          -> WimsId
                                          -> Request
                                          -> WimsHandler Response)
                                         :<|> ((Vault
                                                -> AuthTokenOwner
                                                -> WimsId
                                                -> Request
                                                -> WimsHandler Response)
                                               :<|> ((Vault
                                                      -> ApplicationRole
                                                      -> EligibilityRequest
                                                      -> WimsHandler EligibilityResponse)
                                                     :<|> (Vault
                                                           -> ApplicationRole
                                                           -> WimsId
                                                           -> UserCommentRequestBody
                                                           -> WimsHandler Response)))))))
    Actual type: ServerT TransferBicepsApi WimsHandler|]

servantTypeResult :: T.Text
servantTypeResult =
  [i| (Vault
                  -> ApplicationRole -> WimsId -> WimsHandler Response)
                 :<|> ((Vault
                        -> AuthTokenOwner -> WimsId -> Request -> WimsHandler Response)
                       :<|> ((Vault
                              -> AuthTokenOwner -> RawJSONText Request -> WimsHandler Response)
                             :<|> ((Vault
                                    -> AuthTokenOwner -> WimsId -> Request -> WimsHandler Response)
                                   :<|> ((Vault
                                          -> AuthTokenOwner
                                          -> WimsId
                                          -> Request
                                          -> WimsHandler Response)
                                         :<|> ((Vault
                                                -> AuthTokenOwner
                                                -> WimsId
                                                -> Request
                                                -> WimsHandler Response)
                                               :<|> ((Vault
                                                      -> ApplicationRole
                                                      -> EligibilityRequest
                                                      -> WimsHandler EligibilityResponse)
                                                     :<|> (Vault
                                                           -> ApplicationRole
                                                           -> WimsId
                                                           -> UserCommentRequestBody
                                                           -> WimsHandler Response)))))))
|]

test2 :: ET.ErrorBody
test2 = TE.encodeUtf8
  [i|• Couldn't match expected type: String -> IO ()
              with actual type: IO ()
• The function ‘putStrLn’ is applied to two value arguments,
    but its type ‘String -> IO ()’ has only one
  In a stmt of a 'do' block: putStrLn 43770 "world"
  In the expression: do putStrLn 43770 "world"|]

test2Result :: LBS.ByteString
test2Result = TEL.encodeUtf8
  [i|• Couldn&\#39;t match expected type: <span class="syn-uc-identifier">String</span> <span class="syn-operator">-&gt;</span> <span class="syn-uc-identifier">IO</span> <span class="syn-reserved-name">(</span><span class="syn-reserved-name">)</span>
              with actual type: <span class="syn-uc-identifier">IO</span> <span class="syn-reserved-name">(</span><span class="syn-reserved-name">)</span>
• The function ‘<span class="syn-lc-identifier">putStrLn</span>’ is applied to two value arguments,
    but its type ‘<span class="syn-uc-identifier">String</span> <span class="syn-operator">-&gt;</span> <span class="syn-uc-identifier">IO</span> <span class="syn-reserved-name">(</span><span class="syn-reserved-name">)</span>’ has only one
  In a stmt of a &\#39;do&\#39; block: <span class="syn-lc-identifier">putStrLn</span> <span class="syn-number">43770</span> <span class="syn-string-lit">&quot;world&quot;</span>
  In the expression: <span class="syn-reserved-name">do</span> <span class="syn-lc-identifier">putStrLn</span> <span class="syn-number">43770</span> <span class="syn-string-lit">&quot;world&quot;</span>|]

test3 :: T.Text
test3 =
  [i|• In the type signature:
    handleArchivePone :: _ =>
                         Maybe Graph
                         -> ParsedMessage
                            -> m (Either MessageProcessError MessageProcessingOutputs)
                               -> m (Either MessageProcessError MessageProcessingOutputs)|]

test3Result :: T.Text
test3Result =
  [i|
    handleArchivePone :: _ =>
                         Maybe Graph
                         -> ParsedMessage
                            -> m (Either MessageProcessError MessageProcessingOutputs)
                               -> m (Either MessageProcessError MessageProcessingOutputs)|]

test4 :: T.Text
test4 =
  [i|  In a stmt of a pattern guard for
                 a case alternative:
    Just locationName <- W.twName <$> getTransferringWarehouse wtg|]

test4Result :: T.Text
test4Result = "\n    Just locationName <- W.twName <$> getTransferringWarehouse wtg"
