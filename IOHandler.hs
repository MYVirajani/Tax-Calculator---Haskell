module IOHandler
  ( interactiveMode
  , batchModeFromCSV
  ) where

import DataTypes
import Processing
import Utils
import System.IO (hFlush, stdout)
import Data.List (intercalate)
import Control.Exception (try, SomeException)
import Text.Printf (printf)

-- Default tax brackets (example). You can modify these to fit your tax rules.
defaultBrackets :: [TaxBracket]
defaultBrackets =
  [ TaxBracket 500000 0.05   -- first 500k at 5%
  , TaxBracket 500000 0.10   -- next 500k at 10%
  , TaxBracket 1000000 0.20  -- next 1M at 20%
  , TaxBracket (1/0) 0.30    -- remaining at 30% (limit = Infinity)
  ]

-- Interactive prompt mode
interactiveMode :: IO ()
interactiveMode = do
  putStrLn "=== Tax Calculator (Interactive) ==="
  putStr "Enter your name: "
  hFlush stdout
  name <- getLine
  putStr "Enter your annual income (numbers only): "
  hFlush stdout
  incomeStr <- getLine
  case safeReadDouble incomeStr of
    Nothing -> putStrLn "Invalid number. Exiting."
    Just income -> do
      let tp = TaxPayer name income
          res = processTaxPayer defaultBrackets tp
      displayResult res

displayResult :: TaxResult -> IO ()
displayResult r = do
  putStrLn $ "Tax summary for: " ++ resName r
  putStrLn $ "Annual Income: " ++ formatAmount (resIncome r)
  putStrLn $ "Total Tax: " ++ formatAmount (resTax r)
  putStrLn "Breakdown by slab:"
  mapM_ displaySlab (resBreakdown r)
  where
    displaySlab bd = putStrLn $
      "  Taxed " ++ formatAmount (slabLimit bd)
      ++ " @ " ++ printf "%.2f%%" (slabRate bd * 100)
      ++ " => " ++ formatAmount (slabTax bd)

-- Batch mode: read CSV file with lines "Name,Income"
-- Returns list of TaxResult or prints an error
batchModeFromCSV :: FilePath -> IO ()
batchModeFromCSV path = do
  e <- try (readFile path) :: IO (Either SomeException String)
  case e of
    Left err -> putStrLn $ "Failed to read file: " ++ show err
    Right content -> do
      let ls = filter (not . null) $ map trim $ lines content
          parsed = map parseLine ls
      case sequence parsed of
        Left errMsg -> putStrLn $ "Parse error: " ++ errMsg
        Right tps -> do
          let results = processMany defaultBrackets tps
          mapM_ displayResult results

-- parse "Name,Income" -> Maybe TaxPayer (Left String on error)
parseLine :: String -> Either String TaxPayer
parseLine s =
  case splitComma s of
    [n, incStr] ->
      case safeReadDouble incStr of
        Nothing -> Left $ "Invalid income number: " ++ incStr ++ " in line: " ++ s
        Just inc -> Right $ TaxPayer n inc
    _ -> Left $ "Invalid CSV format (expected Name,Income): " ++ s

-- small helpers
splitComma :: String -> [String]
splitComma = map trim . splitOn ','

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn sep str =
  let (pre, rest) = span (/= sep) str
  in case rest of
       []      -> [pre]
       (_:xs)  -> pre : splitOn sep xs

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\r\n")
