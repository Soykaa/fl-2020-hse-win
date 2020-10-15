module Main where

import PrologParser
import System.IO
import Text.ParserCombinators.Parsec
import System.Environment

parseString :: Parser a -> String -> Either ParseError a
parseString p =
  parse (do r <- p; eof; return r) ""

parseFromFile' :: FilePath -> IO ()
parseFromFile' path = do
  input <- readFile path
  case parseString prog input of
    Left err -> print err
    Right r -> do
      writeFile (path ++ ".out") (show r)


main :: IO ()
main = do
  filename <- getArgs 
  parseFromFile' (head filename)