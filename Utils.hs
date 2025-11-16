module Utils
  ( formatAmount
  , safeReadDouble
  , round2
  ) where

import Text.Read (readMaybe)

-- Format amount for display (two decimal places)
formatAmount :: Double -> String
formatAmount x = "Rs " ++ show (round2 x)

-- Round to 2 decimal places
round2 :: Double -> Double
round2 x = fromIntegral (round (x * 100)) / 100.0

-- Safely parse Double from String
safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe
