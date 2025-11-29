module Language.Binah.Parser.Document where

import Control.Applicative (optional)
import Data.Void (Void)
import Language.Binah.Ast.Content (Content(..))
import Language.Binah.Ast.Tree
import Language.Binah.Parser.Content (parseBodyContents)
import Language.Binah.Parser.Meta (parseMetadataBlock)
import Text.Megaparsec (Parsec, takeRest, takeWhileP)
import Text.Megaparsec.Char (newline)

type Parser = Parsec Void String

parseDocument :: Parser Binah
parseDocument = do
    titleLine <- takeWhileP (Just "title") notLineEnd
    _ <- optional newline
    metas <- parseMetadataBlock
    _ <- optional newline
    body <- takeRest
    let sections = buildSections body
    pure $ Document (Preference titleLine metas) sections
  where
    notLineEnd c = c /= '\n' && c /= '\r'

buildSections :: String -> [Section]
buildSections body =
    case parseBodyContents body of
        [] -> []
        contents -> [Section "Body" contents]