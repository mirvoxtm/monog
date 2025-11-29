module Language.Binah.Parser.Content (parseBodyContents) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Void (Void)
import Language.Binah.Ast.Content
import Language.Binah.Ast.Text (Writing(..))
import Language.Binah.Parser.Text (parseWritings)
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char

type BodyParser = Parsec Void String

-- TODO: Clean this up later asap.

parseBodyContents :: String -> [Content]
parseBodyContents body =
  case runParser (bodyTokensParser <* MP.eof) "body" body of
    Left _ -> []
    Right tokens ->
      let (contents, _) = consumeLevel 0 tokens
      in contents

data BodyToken
  = HeadingToken Int String
  | ArrowListToken Int String
  | CodeBlockToken [String]
  | BulletListToken [String]
  | BreakToken
  | ParagraphToken String
  deriving (Show, Eq)

bodyTokensParser :: BodyParser [BodyToken]
bodyTokensParser = do
  spaceConsumer
  many (blockToken <* spaceConsumer)
  where
  blockToken = headingToken <|> arrowListToken <|> codeBlockToken <|> bulletListToken <|> breakToken <|> paragraphToken

headingToken :: BodyParser BodyToken
headingToken = try $ do
  _ <- takeWhileP (Just "heading indentation") isInlineSpace
  hashes <- some (char '#')
  _ <- char ' '
  rawTitle <- takeWhileP (Just "heading title") (/= '\n')
  _ <- optional newline
  let title = trim rawTitle
  guard (not (null title))
  pure (HeadingToken (length hashes) title)

arrowListToken :: BodyParser BodyToken
arrowListToken = try $ do
  _ <- takeWhileP (Just "arrow list indentation") isInlineSpace
  arrows <- some (char '>')
  guard (length arrows >= 2)
  _ <- char ' '
  rawTitle <- takeWhileP (Just "arrow list title") (/= '\n')
  _ <- optional newline
  let title = trim rawTitle
      level = max 1 (length arrows - 1)
  guard (not (null title))
  pure (ArrowListToken level title)

paragraphToken :: BodyParser BodyToken
paragraphToken = ParagraphToken . normalizeParagraphText <$> some paragraphLine

paragraphLine :: BodyParser String
paragraphLine = try $ do
  preview <- lookAhead (takeWhileP (Just "paragraph preview") (/= '\n'))
  guard (not (isHeadingLine preview))
  guard (not (isArrowLine preview))
  guard (not (isBulletLine preview))
  guard (not (isCodeFenceLine preview))
  line <- takeWhileP (Just "paragraph line") (/= '\n')
  let trimmedLine = trim line
  guard (not (null trimmedLine))
  _ <- optional newline
  pure line

bulletListToken :: BodyParser BodyToken
bulletListToken = try $ do
  first <- lookAhead (takeWhileP (Just "bullet preview") (/= '\n'))
  guard (isBulletLine first)
  lines <- some $ try $ do
    preview <- lookAhead (takeWhileP (Just "bullet preview") (/= '\n'))
    guard (isBulletLine preview || isContinuationLine preview)
    line <- takeWhileP (Just "bullet line") (/= '\n')
    _ <- optional newline
    pure line
  pure (BulletListToken lines)

codeBlockToken :: BodyParser BodyToken
codeBlockToken = try $ do
  _ <- takeWhileP (Just "code fence indentation") isInlineSpace
  _ <- string "```"
  _ <- takeWhileP (Just "code fence header") (/= '\n')
  _ <- optional newline
  lines <- manyTill (takeWhileP (Just "code line") (/= '\n') <* optional newline) (try $ do
    _ <- takeWhileP (Just "code fence indentation") isInlineSpace
    _ <- string "```"
    _ <- optional newline
    pure ())
  pure (CodeBlockToken lines)

breakToken :: BodyParser BodyToken
breakToken = try $ do
  _ <- takeWhileP (Just "break indentation") isInlineSpace
  raw <- takeWhileP (Just "break") (/= '\n')
  _ <- optional newline
  let s = trim raw
  guard (s == "___" || s == "---")
  pure BreakToken

spaceConsumer :: BodyParser ()
spaceConsumer = MP.skipMany (blankLine <|> trailingSpaces)

blankLine :: BodyParser ()
blankLine = try $ do
  _ <- hspace
  _ <- newline
  pure ()

trailingSpaces :: BodyParser ()
trailingSpaces = try $ do
  _ <- hspace1
  MP.lookAhead MP.eof
  pure ()

isHeadingLine :: String -> Bool
isHeadingLine raw =
  let stripped = dropWhile isInlineSpace raw
      (hashes, rest) = span (== '#') stripped
  in not (null hashes) && case rest of
      (' ':xs) -> not (null (trim xs))
      _        -> False

isArrowLine :: String -> Bool
isArrowLine raw =
  let stripped = dropWhile isInlineSpace raw
      (arrows, rest) = span (== '>') stripped
  in length arrows >= 2 && case rest of
      (' ':xs) -> not (null (trim xs))
      _        -> False

isInlineSpace :: Char -> Bool
isInlineSpace c = c == ' ' || c == '\t'

consumeLevel :: Int -> [BodyToken] -> ([Content], [BodyToken])
consumeLevel _ [] = ([], [])
consumeLevel level tokens@(HeadingToken lvl _ : _)
  | lvl <= level = ([], tokens)
consumeLevel level tokens@(ArrowListToken lvl _ : _)
  | lvl <= level = ([], tokens)
consumeLevel level (HeadingToken lvl title : rest) =
  let (children, remaining) = consumeLevel lvl rest
      chapter = Chapter lvl title children
      (siblings, finalRest) = consumeLevel level remaining
  in (chapter : siblings, finalRest)
consumeLevel level (ArrowListToken lvl title : rest) =
  let (children, remaining) = consumeLevel lvl rest
      arrowList = ArrowList lvl title children
      (siblings, finalRest) = consumeLevel level remaining
  in (arrowList : siblings, finalRest)
consumeLevel level (BulletListToken lines : rest) =
  let items = parseBulletList lines
      (siblings, finalRest) = consumeLevel level rest
  in (items ++ siblings, finalRest)

consumeLevel level (CodeBlockToken lines : rest) =
  let codeText = unlines lines
      codeNode = CodeBlock codeText
      (siblings, remaining) = consumeLevel level rest
  in (codeNode : siblings, remaining)

consumeLevel level (BreakToken : rest) =
  let (siblings, remaining) = consumeLevel level rest
  in (Break : siblings, remaining)
consumeLevel level (ParagraphToken text : rest) =
  case paragraphNode text of
    Nothing -> consumeLevel level rest
    Just paragraph ->
      let (siblings, remaining) = consumeLevel level rest
      in (paragraph : siblings, remaining)

paragraphNode :: String -> Maybe Content
paragraphNode txt =
  case parseParagraph txt of
    [] -> Nothing
    [Plain s] | trim s == "::Summary::" -> Just Summary
    writings -> Just (Paragraph writings)

parseParagraph :: String -> [Writing]
parseParagraph text =
  case runParser parseWritings "paragraph" text of
    Left _   -> [Plain text]
    Right [] -> [Plain text]
    Right ws -> ws

normalizeParagraphText :: [String] -> String
normalizeParagraphText =
  unwords . map trim . filter (not . null . trim)

parseBulletList :: [String] -> [Content]
parseBulletList lines =
  let items = splitBulletItems lines
      bulletItems = map (toBulletItem) items
  in [BulletList 1 bulletItems]

splitBulletItems :: [String] -> [[Content]]
splitBulletItems [] = []
splitBulletItems (line:rest) =
  if isBulletLine line
    then
      let (itemLines, remaining) = span (not . isBulletLine) rest
          -- remove bullet prefix from first line and dedent continuation lines
          first = stripBulletMarker line
          continuations = map stripContinuation itemLines
          itemStr = unlines (first : continuations)
          -- parse paragraph writings for the item
          ws = parseParagraph itemStr
          itemContent = [Paragraph ws]
      in itemContent : splitBulletItems remaining
    else splitBulletItems rest

isBulletLine :: String -> Bool
isBulletLine raw =
  let stripped = dropWhile isInlineSpace raw
  in case stripped of
      ('-':' ':_) -> True
      ('*':' ':_) -> True
      _           -> False

isContinuationLine :: String -> Bool
isContinuationLine raw =
  case raw of
    (c:_) -> isInlineSpace c
    _     -> False

stripBulletMarker :: String -> String
stripBulletMarker raw =
  let stripped = dropWhile isInlineSpace raw
  in case stripped of
    ('-':' ':xs) -> xs
    ('*':' ':xs) -> xs
    _ -> raw

stripContinuation :: String -> String
stripContinuation = dropWhile isInlineSpace

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

toBulletItem :: [Content] -> Content
toBulletItem cs =
  case cs of
    [Paragraph ws] -> Paragraph ws
    _ -> -- Flatten nested Paragraphs into a single Paragraph
      let ws = concat [ ws' | Paragraph ws' <- cs ]
      in Paragraph ws

isCodeFenceLine :: String -> Bool
isCodeFenceLine raw =
  let stripped = dropWhile isInlineSpace raw
  in take 3 stripped == "```"
