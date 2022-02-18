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
import           Text.Blaze.Html5.Attributes as A hiding (span)
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
  | (ixTxt, pathTxt) <- T.dropWhile (== '-') <$> T.break (== '-') txt
  , Just ix <- readMaybe $ T.unpack ixTxt
  = Just (ix, TE.encodeUtf8 $ T.replace "%" "/" pathTxt)
  | otherwise = Nothing

newtype ErrorCache =
  MkErrorCache
    { fileErrors :: M.Map ET.FilePath [(ET.FileError, RenderedHtml)] }

emptyErrorCache :: ErrorCache
emptyErrorCache = MkErrorCache { fileErrors = mempty }

getModuleName :: ET.FileError -> BS8.ByteString
getModuleName fError
  = fromMaybe (ET.filepath fError) $ ET.moduleName fError

renderViewport :: ET.FileError -> RenderedHtml
renderViewport fileErr = renderHtml $ do
  let err = ET.errorMsg fileErr
      modName = getModuleName fileErr
      filePath = toValue . decodeUtf8 $ ET.filepath fileErr
  div ! class_ "error-heading" $ do
    div ! class_ "nav-arrows" $ do
      div ! A.id "nav-up-arrow" ! A.title "Previous (Shift+UpArrow)"
        $ preEscapedToMarkup ("&#8249;" :: T.Text)
      div ! A.id "nav-down-arrow" ! A.title "Next (Shift+DownArrow)"
        $ preEscapedToMarkup ("&#8250;" :: T.Text)
    case ET.errorType err of
      ET.Error -> span ! class_ "severity severity-error" $ "Error"
      ET.Warning -> span ! class_ "severity severity-warning" $ "Warning"
    span ! class_ "mod-name" ! A.title filePath $
      toMarkup $ decodeUtf8 modName
    span ! class_ "location" $
      "(" <> renderLocation (ET.fileLocation err) <> ")"
  div ! class_ "error-body" $
    toMarkup . decodeUtf8 $ ET.body err
  div ! class_ "error-caret" $
    toMarkup . decodeUtf8 $ ET.caret err

renderSidebar :: ErrorCache -> RenderedHtml
renderSidebar errorCache = renderHtml $ do
  div ! class_ "counts-wrapper" $
    div ! class_ "counts" $ do
      let (warns, errs) = List.partition ET.isWarning . foldMap (fmap fst)
                        $ fileErrors errorCache
          numErrors = length errs
          numWarns = length warns
          pluralize n | n == 1 = ""
                      | otherwise = "s"
      span ! class_ "error-count" $
        toMarkup numErrors <> " Error" <> pluralize numErrors
      span ", "
      span ! class_ "warn-count" $
        toMarkup numWarns <> " Warning" <> pluralize numWarns
  div ! class_ "errors-list"
      $ traverse_ fileGroup
      $ fileErrors errorCache
  where
    fileGroup fErrors = do
      let mFError = fst <$> listToMaybe fErrors
          mModName = getModuleName <$> mFError
          filePath = toValue $ foldMap (decodeUtf8 . ET.filepath) mFError
          errHtml = fmap errorPreviewHtml
                  . (`zip` [0..])
                  $ fst <$> fErrors
      div ! class_ "file-group" $ do
        span ! class_ "file-name" ! A.title filePath $
          toMarkup (decodeUtf8 $ fold mModName)
        div ! class_ "errors-for-file" $
          mconcat errHtml

errorPreviewHtml :: (ET.FileError, Word) -> Html
errorPreviewHtml (fileError, errIdx) = do
  let errMsg = ET.errorMsg fileError
      clss = case ET.errorType errMsg of
               ET.Error -> "error"
               ET.Warning -> "error warning"
      errorId = mkErrorId fileError errIdx
      ixAttr = renderErrorId errorId
      (sevClass, errorTypeTxt)
        = case ET.errorType errMsg of
            ET.Error -> ("severity-error", "Error")
            ET.Warning -> ("severity-warning", "Warning")
   in div ! A.id ixAttr ! class_ clss $ do
        div ! class_ "error-preview-header" $ do
          span ! class_ ("severity " <> sevClass) $ errorTypeTxt
          span ! class_ "location" $ renderLocation (ET.fileLocation errMsg)
        div ! class_ "error-preview-container" $ do
          div ! class_ "error-preview" $
            renderErrorPreview (ET.body errMsg)

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

-- | Produces a command that can be copy/pasted in the vim command prompt to
-- go to jump to the location of an error.
-- mkVimCommand :: ET.FileError -> AttributeValue
-- mkVimCommand err =
--   "e " <> toValue (decodeUtf8 $ ET.filepath err)
--   <> " | cal cursor(" <> lineNum <> "," <> colNum <> ")"
--   where
--     lineNum = toValue . ET.lineNum . ET.fileLocation $ ET.errorMsg err
--     colNum = toValue . ET.colNum . ET.fileLocation $ ET.errorMsg err

decodeUtf8 :: BS8.ByteString -> T.Text
decodeUtf8 = TE.decodeUtf8With TE.ignore
