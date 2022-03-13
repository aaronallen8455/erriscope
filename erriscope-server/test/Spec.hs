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
    it "handles quotes inside In statement" $ do
      let res = renderHtml $ E.renderErrorBody test5
      res `shouldBe` test5Result

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

test5 :: ET.ErrorBody
test5 = TE.encodeUtf8
  [i|  In an equation for ‘exampleEdi855’:
      exampleEdi855
        = Edi855
            {edi855GroupNumber = FunctionalGroupControlNumber "000000001",
             edi855TransactionNumber = TransactionSetControlNumber "000000001",
             edi855TransactionPurpose = Origination,
             edi855AcknowledgmentType = AcknowledgeProductReplenishment,
             edi855OrderId = EdiOrderId "00X000000",
             edi855AcknowledgmentDate = AcknowledgementDateTime exampleDateTime,
             edi855EstimatedDeliveryDate = Just
                                             $ EstimatedDeliveryDateTime exampleDateTime,
             edi855CurrentDeliveryDate = Just
                                           $ CurrentDeliveryDateTime exampleDateTime,
             edi855CurrentShipDate = Just $ CurrentShipDateTime exampleDateTime,
             edi855RequestedPickupDateTime = Just
                                               $ RequestedPickupDateTime exampleDateTime,
             edi855CancellationDateTime = Just
                                            $ CancellationDateTime exampleDateTime,
             edi855CarrierDetails = Just exampleEdiCarrier,
             edi855CarrierEquipment = Just exampleEdiCarrierEquipment,
             edi855ItemData = [exampleEdiItem],
             edi855ServiceDetails = Just exampleEdiService,
             edi855SupplierPickupReferenceNumber = Just
                                                     exampleSupplierPickupReferenceNumber,
             edi855OriginFacilityAliasId = Just exampleOriginFacilityAliasId}|]

test5Result :: LBS.ByteString
test5Result = [i|  In an equation for ‘<span class="syn-lc-identifier">exampleEdi855</span>’:
      <span class="syn-lc-identifier">exampleEdi855</span>
        <span class="syn-operator">=</span> <span class="syn-uc-identifier">Edi855</span>
            <span class="syn-reserved-name">{</span><span class="syn-lc-identifier">edi855GroupNumber</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">FunctionalGroupControlNumber</span> <span class="syn-string-lit">&quot;000000001&quot;</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855TransactionNumber</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">TransactionSetControlNumber</span> <span class="syn-string-lit">&quot;000000001&quot;</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855TransactionPurpose</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Origination</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855AcknowledgmentType</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">AcknowledgeProductReplenishment</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855OrderId</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">EdiOrderId</span> <span class="syn-string-lit">&quot;00X000000&quot;</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855AcknowledgmentDate</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">AcknowledgementDateTime</span> <span class="syn-lc-identifier">exampleDateTime</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855EstimatedDeliveryDate</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span>
                                             <span class="syn-operator">$</span> <span class="syn-uc-identifier">EstimatedDeliveryDateTime</span> <span class="syn-lc-identifier">exampleDateTime</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855CurrentDeliveryDate</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span>
                                           <span class="syn-operator">$</span> <span class="syn-uc-identifier">CurrentDeliveryDateTime</span> <span class="syn-lc-identifier">exampleDateTime</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855CurrentShipDate</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span> <span class="syn-operator">$</span> <span class="syn-uc-identifier">CurrentShipDateTime</span> <span class="syn-lc-identifier">exampleDateTime</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855RequestedPickupDateTime</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span>
                                               <span class="syn-operator">$</span> <span class="syn-uc-identifier">RequestedPickupDateTime</span> <span class="syn-lc-identifier">exampleDateTime</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855CancellationDateTime</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span>
                                            <span class="syn-operator">$</span> <span class="syn-uc-identifier">CancellationDateTime</span> <span class="syn-lc-identifier">exampleDateTime</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855CarrierDetails</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span> <span class="syn-lc-identifier">exampleEdiCarrier</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855CarrierEquipment</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span> <span class="syn-lc-identifier">exampleEdiCarrierEquipment</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855ItemData</span> <span class="syn-operator">=</span> <span class="syn-reserved-name">[</span><span class="syn-lc-identifier">exampleEdiItem</span><span class="syn-reserved-name">]</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855ServiceDetails</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span> <span class="syn-lc-identifier">exampleEdiService</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855SupplierPickupReferenceNumber</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span>
                                                     <span class="syn-lc-identifier">exampleSupplierPickupReferenceNumber</span><span class="syn-reserved-name">,</span>
             <span class="syn-lc-identifier">edi855OriginFacilityAliasId</span> <span class="syn-operator">=</span> <span class="syn-uc-identifier">Just</span> <span class="syn-lc-identifier">exampleOriginFacilityAliasId</span><span class="syn-reserved-name">}</span>|]

-- • No instance for (Real ServiceAmount)
--     arising from a use of ‘toRational’
