module Language.Monog.Ast.Content where

import Language.Monog.Ast.Text (Writing)
import Language.Monog.Ast.Types (Title, File)

{-

    The Content data structure represents the different types of content that can be
    found within a Monog document. It includes paragraphs, which are made up of
    formatted text (Writing), and chapters, which can contain nested content.

-}


data Content = Paragraph [Writing]
             | Url File [Writing]
             | Chapter Int Title [Content]
             | ArrowList Int Title [Content]
             deriving (Show, Eq)