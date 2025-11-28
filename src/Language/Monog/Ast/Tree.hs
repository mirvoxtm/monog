module Language.Monog.Ast.Tree where

import Language.Monog.Ast.Content (Content)
import Language.Monog.Ast.Types (Atom, Title)

{-

    The Monog data structure represents the overall structure of a Monog document,
    which consists of preferences and sections. Each section contains a title and
    multiple paragraphs, and each paragraph is made up of formatted text (Writing).

-}


data Monog = Document Preferences [Section]
             deriving (Show, Eq)

data Preferences = Preference Title [Meta]
                   deriving (Show, Eq)

data Meta = Author [Atom]
          | Institution Atom
          | Subtitle Atom
          | Location Atom
          | Description Atom
          | Mentor Atom
          | Keywords [Atom]
          | Date Atom
          | NoMetadata
          deriving (Show, Eq)

data Section = Section Title [Content]
               deriving (Show, Eq)