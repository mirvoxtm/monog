module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Language.Monog.Ast.Tree (Monog)
import Language.Monog.Converter.Html (toHtml)
import Language.Monog.Parser.Document (parseDocument)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)
import System.IO (IOMode (WriteMode), hPutStr, hSetEncoding, withFile)
import Text.Megaparsec (errorBundlePretty, parse, eof)

parseFileWith :: (Monog -> String) -> FilePath -> String -> Either String String
parseFileWith renderer source text =
  case parse (parseDocument <* eof) source text of
    Left err -> Left (errorBundlePretty err)
    Right res -> Right (renderer res)

convertToHtml :: FilePath -> String -> Either String String
convertToHtml = parseFileWith toHtml

resolvePaths :: [String] -> (FilePath, FilePath)
resolvePaths [] = (defaultInput, defaultOutput)
resolvePaths [inp] = (inp, defaultOutputFor inp)
resolvePaths (inp:out:_) = (inp, out)

defaultInput :: FilePath
defaultInput = "readme.mks"

defaultOutput :: FilePath
defaultOutput = "readme.html"

defaultOutputFor :: FilePath -> FilePath
defaultOutputFor path =
  let replaced = replaceExtension path ".html"
  in if null replaced then defaultOutput else replaced

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  let (inputPath, outputPath) = resolvePaths args
  content <- readFile inputPath
  case convertToHtml inputPath content of
    Left err -> do
      putStrLn "Failed to parse document:\n"
      putStrLn err
      exitFailure
    Right html ->
      withFile outputPath WriteMode $ \handle -> do
        hSetEncoding handle utf8
        hPutStr handle html