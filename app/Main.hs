module Main (main) where

import AST (runProgram)
import Parser (parseProgram)
import System.Environment (getArgs)
import Text.Parsec (parse)

main :: IO ()
main = do
  args <- getArgs
  let f = (case args of
        (x : _) -> x
        [] -> error "Please supply a file to run")

  content <- readFile f
  let program = either (error . show) id $ parse parseProgram f content
  let result = runProgram program
  mapM_ print $ reverse result
