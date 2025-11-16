module Processing
  ( calculateTax
  , taxBreakdown
  , processTaxPayer
  , processMany
  ) where

import DataTypes
import Data.List (foldl')
-- import Control.Parallel.Strategies (parList, rseq, using)

-- Recursively compute tax across ordered brackets.
-- brackets: list of TaxBracket describing slab sizes and rates, in ascending order.
-- income: amount to compute tax on.
calculateTax :: [TaxBracket] -> Double -> Double
calculateTax brackets income = go brackets income 0.0
  where
    go [] _ acc = acc
    go (TaxBracket lim r : bs) remaining acc
      | remaining <= 0 = acc
      | otherwise =
          let taxed   = min lim remaining
              taxPart = taxed * r
          in go bs (remaining - taxed) (acc + taxPart)

-- Return a detailed breakdown of how tax was computed per slab (pure)
taxBreakdown :: [TaxBracket] -> Double -> [TaxBreakdown]
taxBreakdown brackets income = reverse $ snd $ foldl' step (income, []) brackets
  where
    step (remaining, acc) (TaxBracket lim r)
      | remaining <= 0 = (0, acc)
      | otherwise =
          let taxed = min lim remaining
              t = taxed * r
              bd = TaxBreakdown taxed r t
          in (remaining - taxed, bd : acc)

-- Process a single taxpayer to produce a TaxResult
processTaxPayer :: [TaxBracket] -> TaxPayer -> TaxResult
processTaxPayer brackets tp =
  let inc = tpIncome tp
      bd  = taxBreakdown brackets inc
      total = sum (map slabTax bd)
  in TaxResult (tpName tp) inc total bd

-- Process many taxpayers; demonstrates a pure parallel map (using Strategies)
-- To enable parallelism at runtime pass +RTS -N (e.g. -N2) when running the binary.
-- processMany :: [TaxBracket] -> [TaxPayer] -> [TaxResult]
-- processMany brackets tps =
--   let results = map (processTaxPayer brackets) tps
--   in results `using` parList rseq
processMany :: [TaxBracket] -> [TaxPayer] -> [TaxResult]
processMany brackets tps = map (processTaxPayer brackets) tps
