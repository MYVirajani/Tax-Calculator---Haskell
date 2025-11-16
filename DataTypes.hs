{-# LANGUAGE DeriveGeneric #-}
module DataTypes
  ( TaxBracket(..)
  , TaxPayer(..)
  , TaxBreakdown(..)
  , TaxResult(..)
  ) where

import GHC.Generics (Generic)

-- A tax bracket: amount limit (for the slab) and rate (as fraction, e.g. 0.05 = 5%)
data TaxBracket = TaxBracket
  { limit :: Double   -- amount in this bracket (or remaining amount for top slab)
  , rate  :: Double   -- e.g. 0.05 for 5%
  } deriving (Show, Eq, Generic)

-- Simple taxpayer record
data TaxPayer = TaxPayer
  { tpName   :: String
  , tpIncome :: Double
  } deriving (Show, Eq, Generic)

-- Break down tax for a single slab
data TaxBreakdown = TaxBreakdown
  { slabLimit :: Double   -- amount taxed at this slab
  , slabRate  :: Double   -- rate for this slab
  , slabTax   :: Double   -- computed tax for this slab
  } deriving (Show, Eq, Generic)

-- Overall result for a taxpayer
data TaxResult = TaxResult
  { resName      :: String
  , resIncome    :: Double
  , resTax       :: Double
  , resBreakdown :: [TaxBreakdown]
  } deriving (Show, Eq, Generic)
