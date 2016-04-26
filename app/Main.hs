module Main where

import Data.String.Strip
import Console.Driver

main :: IO ()
main = do
  Console.Driver.greeting
  putStrLn "Are you X or O?"
  character <- getLine
  putStrLn $ "You chose " ++ character
