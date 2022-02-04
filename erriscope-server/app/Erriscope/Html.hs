{-# LANGUAGE RecordWildCards #-}
module Erriscope.Html
  ( HtmlCache(..)
  , emptyHtmlCache
  , mockHtmlCache
  , renderPayload
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           Prelude hiding (div, span)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (span)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Erriscope.Types as ET

data HtmlCache =
  MkHtmlCache
    { sidebarHtml :: LBS.ByteString
    , viewportHtml :: IM.IntMap LBS.ByteString
    }

emptyHtmlCache :: HtmlCache
emptyHtmlCache =
  MkHtmlCache
    { sidebarHtml = mempty
    , viewportHtml = mempty
    }

mockHtmlCache :: HtmlCache
mockHtmlCache = renderPayload ET.mockPayload

renderPayload :: ET.Payload -> HtmlCache
renderPayload payload = MkHtmlCache{..}
  where
    viewportHtml = IM.fromList . zip [0..] $ do
      fileErr <- ET.fileErrors payload
      err <- ET.errors fileErr
      pure . renderHtml $ do
        div ! class_ "error-heading" $ do
          span ! class_ "file-path" $
            toMarkup . decodeUtf8 $ ET.filepath fileErr
          span ! class_ "location" $ renderLocation (ET.fileLocation err)
        div ! class_ "error-body" $
          toMarkup . decodeUtf8 $ ET.body err

    sidebarHtml = renderHtml $ do
      div ! class_ "errors-list" $
        void . (`evalStateT` 0) . traverse fileGroup $ ET.fileErrors payload

    fileGroup fileErrors = do
      errHtml <- traverse (state . errorHtml) (ET.errors fileErrors)
      lift $
        div ! class_ "file-group" $ do
          span ! class_ "file-name" $ toMarkup (decodeUtf8 $ ET.filename fileErrors)
          div ! class_ "errors-for-file" $
            mconcat errHtml

errorHtml :: ET.ErrorMsg -> Int -> (Html, Int)
errorHtml errMsg errIdx = do
  let clss = case ET.errorType errMsg of
               ET.Error -> "error"
               ET.Warning -> "error warning"
      clss' = if errIdx == 0
                 then clss <> " selected"
                 else clss
   in ( div ! class_ clss' ! dataAttribute "index" (toValue errIdx) $ do
          span ! class_ "location" $ renderLocation (ET.fileLocation errMsg)
          div ! class_ "error-preview" $ renderErrorPreview (ET.body errMsg)
      , errIdx + 1
      )

renderLocation :: ET.Location -> Html
renderLocation loc =
  "Line " <> toMarkup (ET.lineNum loc) <> ":" <> toMarkup (ET.colNum loc)

renderErrorPreview :: BS8.ByteString -> Html
renderErrorPreview =
  toMarkup . decodeUtf8 . BS8.unlines . take 4 . BS8.lines

decodeUtf8 :: BS8.ByteString -> T.Text
decodeUtf8 = TE.decodeUtf8With TE.ignore
