module Language.Binah.Ast.Content where

import Language.Binah.Ast.Text (Writing)
import Language.Binah.Ast.Types (Title, File)

{-

    The Content data structure represents the different types of content that can be
    found within a Binah document. It includes paragraphs, which are made up of
    formatted text (Writing), and chapters, which can contain nested content.

-}


data Content = Paragraph [Writing]
             | Url File [Writing]
             | Chapter Int Title [Content]
             | ArrowList Int Title [Content]
             | CodeBlock String
             | BulletList Int [Content]
             | Break
             | Summary
             deriving (Show, Eq)