module Erriscope.Html
  ( ErrorCache(..)
  , emptyErrorCache
  , renderViewport
  , renderSidebar
  , parseErrorId
  , labeledCodeBlock
  , getIndentedPortion
  , renderErrorBody
  , inCodeBlock
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (isSpace)
import           Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           Data.Word
import           Prelude hiding (div, span)
import qualified Prelude
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes as A hiding (span)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Read (readMaybe)

import           Erriscope.Html.SyntaxHighlighting (highlightSyntax)
import qualified Erriscope.Types as ET

type RenderedHtml = LBS.ByteString

type ErrorId = (Word, ET.FilePath)

parseErrorId :: BS8.ByteString -> Maybe ErrorId
parseErrorId bs
  | (ixTxt, path) <- BS8.dropWhile (== '-') <$> BS8.break (== '-') bs
  , Just ix <- readMaybe $ BS8.unpack ixTxt
  -- TODO Find something better than this hack
  = Just (ix, path)
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
  div ! class_ "error-body" $ do
    renderErrorBody $ ET.body err
    p $ renderCaret err

-- | Create HTML for the caret portion of the error.
renderCaret :: ET.ErrorMsg -> Html
renderCaret err = highlight . decodeUtf8 $ ET.caret err
  where
    highlight inp =
      case T.lines inp of
        [l1, l2, l3] -> do
          doLine toMarkup l1
          "\n"
          doLine highlightSyntax l2
          "\n"
          doLine highlightCaret l3
        _ -> toMarkup inp
    doLine f ln =
      let (before, after) = T.break (== '|') ln
       in do
         span ! class_ "caret-gutter" $ toMarkup before <> "|"
         f $ T.drop 1 after
    highlightCaret l =
      let (before, c) = T.span isSpace l
          (caret, rest) = T.span (== '^') c
          caretClass = case ET.errorType err of
                         ET.Error -> "caret-error"
                         ET.Warning -> "caret-warning"
       in toMarkup before
       <> (span ! class_ caretClass) (toMarkup caret)
       <> toMarkup rest

renderErrorBody :: ET.ErrorBody -> Html
renderErrorBody = replaceQuotes . decodeUtf8
  where
    highlight = highlightCodeBlock
      [ startsWithColon
      , inCodeBlock
      -- first character chopped off to account for differing capitalization
      , labeledCodeBlock "xpected type:"
      , labeledCodeBlock "ctual type:"
      , labeledCodeBlock "ariable not in scope:"
      , labeledCodeBlock "To import instances alone, use:"
      , labeledCodeBlock "elevant bindings include"
      , labeledCodeBlock "the inferred type of"
      , labeledCodeBlock "rom the context:"
      , labeledCodeBlock "ound by the type signature for:"
      , labeledCodeBlock "a pattern with constructor:"
      ]
    replaceQuotes = foldMap go . T.split (== '‘')
    go t
      | [codeSnippet, rest] <- T.split (== '’') t
      = toMarkup '‘' <> highlightSyntax codeSnippet <> toMarkup '’'
     <> highlight rest
    go t = highlight t

-- | If a closing quote is followed immediately by a colon then capture that
-- as a code block
-- ex: In an equation for ‘exampleEdi855’: ...
startsWithColon :: T.Text -> Maybe (T.Text, T.Text)
startsWithColon inp = do
  (':', rest) <- T.uncons inp
  -- This could be a problem because the indentation delimiter will not
  -- function properly without the complete line of preceding text which is not
  -- available here. It's probably fine though.
  pure (":", rest)

-- | Identify code snippets within the input and do syntax highlighting.
highlightCodeBlock :: [T.Text -> Maybe (T.Text, T.Text)] -> T.Text -> Html
highlightCodeBlock (fn:preds) inp =
  fromMaybe (highlightCodeBlock preds inp) $ do
    (before, codeBlock) <- fn inp
    (inCode, rest) <- getIndentedPortion before codeBlock
    pure $ highlightCodeBlock preds before
        <> highlightSyntax inCode
        <> highlightCodeBlock (fn:preds) rest
highlightCodeBlock [] inp = toMarkup inp

-- | Captures the start of code blocks prefixed by "In ...:" statements
inCodeBlock :: T.Text -> Maybe (T.Text, T.Text)
inCodeBlock inp = do
  let (before, after) = T.breakOn "In " inp
  guard $ not (T.null after)
  let (inStmt, codeBlock) = T.span (\c -> c /= ':' && c /= '\n') after
  if "In a stmt of " `T.isPrefixOf` inStmt
     then do
       let (inStmt2, inStmt3) = T.span (/= ':') codeBlock
       (':', inCode) <- T.uncons inStmt3
       pure (before <> inStmt <> inStmt2 <> ":", inCode)
     else do
       (':', inCode) <- T.uncons codeBlock
       pure (before <> inStmt <> ":", inCode)

labeledCodeBlock :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
labeledCodeBlock herald inp = do
  let (before, after) = T.breakOn herald inp
  guard . not $ T.null after
  inCode <- T.stripPrefix herald after
  pure (before <> herald, inCode)

-- | When an error contains a block of code, the end of that block can usually
-- be determined by finding the next line where the indentation has changed.
getIndentedPortion
  :: T.Text -- ^ Portion of the message before the start of the code block
  -> T.Text
  -> Maybe (T.Text, T.Text)
getIndentedPortion precedingText inp = do
  let (_, afterNewline) = T.breakOnEnd "\n" precedingText
      addToIndentation = T.length afterNewline
      -- use splitOn instead of lines to preserve newlines at the end
      lns = T.splitOn "\n" inp
      getIndentation = T.length . T.takeWhile isSpace
  (prefix, l : rest) <- pure $ Prelude.span T.null lns
  let ind | null prefix = getIndentation l + addToIndentation
          | otherwise = getIndentation l
      ind' = if ind <= 1 then maxBound else ind
      (inBlock, out) =
        break (\x -> not (T.null x) && getIndentation x < ind')
              rest
  -- unlines adds a newline character to the last element of its input
  -- which is accounted for by using intercalate instead
  pure ( (if null out
             then T.intercalate "\n"
             else T.unlines)
         $ prefix ++ l : inBlock
       , T.intercalate "\n" out )

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
errorPreviewHtml (fileError, errIdx) =
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
          renderErrorPreview (ET.body errMsg)

mkErrorId :: ET.FileError -> Word -> ErrorId
mkErrorId err idx = (idx , ET.filepath err)

renderErrorId :: ErrorId -> AttributeValue
renderErrorId (idx, path)
  = toValue idx <> "-" <> toValue (decodeUtf8 path)

renderLocation :: ET.Location -> Html
renderLocation loc =
  "Line " <> toMarkup (ET.lineNum loc) <> ":" <> toMarkup (ET.colNum loc)

renderErrorPreview :: BS8.ByteString -> Html
renderErrorPreview errBody = do
  let lns = BS8.lines errBody
      truncateCls =
        if length lns > 4
           then " truncated"
           else ""
  div ! class_ ("error-preview" <> truncateCls) $
    toMarkup . decodeUtf8 . BS8.unlines $ take 4 lns

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
