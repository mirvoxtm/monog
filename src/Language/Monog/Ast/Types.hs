module Language.Monog.Ast.Types where

type Atom = String
type Title = String
type Color = (Int, Int, Int)

type FileP = String
data File = Image FileP
          | Video FileP
          | Audio FileP
          deriving (Show, Eq)