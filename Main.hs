module Main where

import System.Environment (getArgs)
import IOHandler

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> interactiveMode
    ["-i"] -> interactiveMode
    ["-f", path] -> batchModeFromCSV path
    _  -> putStrLn $ "Usage:\n  (no args) or -i          -> interactive\n  -f <csvfile>             -> batch CSV mode (Name,Income per line)"
