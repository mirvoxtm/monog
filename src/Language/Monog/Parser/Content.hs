module Language.Monog.Parser.Content
  ( parseBodyContents
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Void (Void)
import Language.Monog.Ast.Content
import Language.Monog.Ast.Text (Writing(..))
import Language.Monog.Parser.Text (parseWritings)
import Text.Megaparsec (Parsec, lookAhead, optional, runParser, some, takeWhileP, try, many)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, hspace, hspace1, newline)

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
  | ParagraphToken String
  deriving (Show, Eq)

bodyTokensParser :: BodyParser [BodyToken]
bodyTokensParser = do
  spaceConsumer
  many (blockToken <* spaceConsumer)
  where
    blockToken = headingToken <|> arrowListToken <|> paragraphToken

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
  line <- takeWhileP (Just "paragraph line") (/= '\n')
  let trimmedLine = trim line
  guard (not (null trimmedLine))
  _ <- optional newline
  pure line

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

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
