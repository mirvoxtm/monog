module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Language.Binah.Ast.Tree (Binah)
import Language.Binah.Converter.Html (toHtml)
import Language.Binah.Parser.Document (parseDocument)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)
import System.IO (IOMode (WriteMode), hPutStr, hSetEncoding, withFile)
import Text.Megaparsec (errorBundlePretty, parse, eof)

parseFileWith :: (Binah -> String) -> FilePath -> String -> Either String String
parseFileWith renderer source text =
  case parse (parseDocument <* eof) source text of
    Left err -> Left (errorBundlePretty err)
    Right res -> Right (renderer res)

convertToHtml :: FilePath -> String -> Either String String
convertToHtml = parseFileWith toHtml

resolvePaths :: [String] -> Either String (FilePath, FilePath)
resolvePaths [] = Left "Usage: binah <input-file>"
resolvePaths [inp] = Right (inp, defaultOutputFor inp)
resolvePaths (_:_:_) = Left "Usage: binah <input-file>"

defaultOutputFor :: FilePath -> FilePath
defaultOutputFor path = replaceExtension path ".html"

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  case resolvePaths args of
    Left err -> do
      putStrLn err
      exitFailure
    Right (inputPath, outputPath) -> do
      content <- readFile inputPath
      case convertToHtml inputPath content of
        Left err -> do
          putStrLn "Failed to parse document:\n"
          putStrLn err
          exitFailure
        Right html -> do
          putStrLn $ "Document parsed at " ++ outputPath ++ "\n"
          withFile outputPath WriteMode $ \handle -> do
            hSetEncoding handle utf8
            hPutStr handle html