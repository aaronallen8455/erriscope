module Erriscope.Html
  ( ErrorCache(..)
  , emptyErrorCache
  , renderViewport
  , renderSidebar
  , parseErrorId
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import           Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           Data.Word
import           Prelude hiding (div, span)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (span)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Read (readMaybe)

import qualified Erriscope.Types as ET

type RenderedHtml = LBS.ByteString

-- The selected error needs to be known on the server. Perhaps have an identifier
-- for errors. Could be the filename + numeric index. Then the rendered viewport
-- html can be keyed on that identifier and it can be placed in the preview html
-- as a data attribute. The identifier of the selected error can be kept as a
-- separate field which is used during rendering the sidebar html.

type ErrorId = (Word, ET.FilePath)

parseErrorId :: T.Text -> Maybe ErrorId
parseErrorId txt
  | [ixTxt, pathTxt] <- T.split (== '-') txt
  , Just ix <- readMaybe $ T.unpack ixTxt
  = Just (ix, TE.encodeUtf8 $ T.replace "%" "/" pathTxt)
  | otherwise = Nothing

data ErrorCache =
  MkErrorCache
    { fileErrors :: M.Map ET.FilePath [(ET.FileError, RenderedHtml)]
    , selectedError :: Maybe ErrorId
    }

emptyErrorCache :: ErrorCache
emptyErrorCache =
  MkErrorCache
    { fileErrors = mempty
    , selectedError = Nothing
    }

-- mockHtmlCache :: HtmlCache
-- mockHtmlCache = renderPayload ET.mockPayload

renderViewport :: ET.FileError -> RenderedHtml
renderViewport fileErr = renderHtml $ do
  -- TODO show whether it's an error or warning
  let err = ET.errorMsg fileErr
  div ! class_ "error-heading" $ do
    span ! class_ "file-path" $
      toMarkup . decodeUtf8 $ ET.filepath fileErr
    span ! class_ "location" $ renderLocation (ET.fileLocation err)
  div ! class_ "error-body" $
    toMarkup . decodeUtf8 $ ET.body err
  div ! class_ "error-caret" $
    toMarkup . decodeUtf8 $ ET.caret err

renderSidebar :: ErrorCache -> RenderedHtml
renderSidebar errorCache = renderHtml $ do
  div ! class_ "errors-list"
      $ traverse_ fileGroup
      $ fileErrors errorCache
  where
    fileGroup fErrors = do
      let curSelected = selectedError errorCache
          mFError = fst <$> listToMaybe fErrors
          mModName = (ET.moduleName =<< mFError)
                 <|> (ET.filepath <$> mFError)
          errHtml = fmap (errorHtml curSelected)
                  . (`zip` [0..])
                  $ fst <$> fErrors
      div ! class_ "file-group" $ do
        span ! class_ "file-name" $
          toMarkup (decodeUtf8 $ fold mModName)
        div ! class_ "errors-for-file" $
          mconcat errHtml

errorHtml :: Maybe ErrorId -> (ET.FileError, Word) -> Html
errorHtml mCurSelected (fileError, errIdx) = do
  let errMsg = ET.errorMsg fileError
      clss = case ET.errorType errMsg of
               ET.Error -> "error"
               ET.Warning -> "error warning"
      clss' = if mCurSelected == Just errorId
                 then clss <> " selected"
                 else clss
      errorId = mkErrorId fileError errIdx
      ixAttr = renderErrorId errorId
      (sevClass, errorTypeTxt)
        = case ET.errorType errMsg of
            ET.Error -> ("severity-error", "Error")
            ET.Warning -> ("severity-warning", "Warning")
   in div ! class_ clss' ! dataAttribute "index" ixAttr $ do
        div ! class_ "error-preview-header" $ do
          span ! class_ ("severity " <> sevClass) $ errorTypeTxt
          span ! class_ "location" $ renderLocation (ET.fileLocation errMsg)
        div ! class_ "error-preview" $ renderErrorPreview (ET.body errMsg)

mkErrorId :: ET.FileError -> Word -> ErrorId
mkErrorId err idx =
  ( idx
  -- replace '/' so that it can be used as a url param
  , mconcat . List.intersperse "%" . BS8.split '/' $ ET.filepath err
  )

renderErrorId :: ErrorId -> AttributeValue
renderErrorId (idx, path)
  = toValue idx <> "-" <> toValue (decodeUtf8 path)

renderLocation :: ET.Location -> Html
renderLocation loc =
  "Line " <> toMarkup (ET.lineNum loc) <> ":" <> toMarkup (ET.colNum loc)

renderErrorPreview :: BS8.ByteString -> Html
renderErrorPreview =
  toMarkup . decodeUtf8 . BS8.unlines . take 4 . BS8.lines

decodeUtf8 :: BS8.ByteString -> T.Text
decodeUtf8 = TE.decodeUtf8With TE.ignore
