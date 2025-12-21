{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module WebServer where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON, encode, decode, object, (.=))
import GHC.Generics
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import DataTypes
import Processing
import Utils

-- JSON request/response types
data TaxRequest = TaxRequest
  { reqName :: String
  , reqIncome :: Double
  , reqDeductions :: Maybe Double
  } deriving (Show, Generic)

instance FromJSON TaxRequest
instance ToJSON TaxRequest

data TaxResponse = TaxResponse
  { respName :: String
  , respIncome :: Double
  , respDeductions :: Double
  , respTaxableIncome :: Double
  , respTotalTax :: Double
  , respBreakdown :: [TaxBreakdown]
  } deriving (Show, Generic)

instance ToJSON TaxResponse
instance FromJSON TaxResponse

instance ToJSON TaxBreakdown
instance FromJSON TaxBreakdown

data BatchRequest = BatchRequest
  { batchTaxpayers :: [TaxRequest]
  } deriving (Show, Generic)

instance FromJSON BatchRequest
instance ToJSON BatchRequest

data BatchResponse = BatchResponse
  { batchResults :: [TaxResponse]
  } deriving (Show, Generic)

instance ToJSON BatchResponse
instance FromJSON BatchResponse

-- Default tax brackets
defaultBrackets :: [TaxBracket]
defaultBrackets =
  [ TaxBracket 500000 0.05
  , TaxBracket 500000 0.10
  , TaxBracket 1000000 0.20
  , TaxBracket (1/0) 0.30
  ]

-- Convert TaxRequest to TaxResponse
processTaxRequest :: [TaxBracket] -> TaxRequest -> TaxResponse
processTaxRequest brackets req =
  let name = reqName req
      income = reqIncome req
      deductions = maybe 0.0 id (reqDeductions req)
      taxableIncome = max 0 (income - deductions)
      breakdown = taxBreakdown brackets taxableIncome
      totalTax = sum (map slabTax breakdown)
  in TaxResponse
      { respName = name
      , respIncome = income
      , respDeductions = deductions
      , respTaxableIncome = taxableIncome
      , respTotalTax = totalTax
      , respBreakdown = breakdown
      }

-- Start web server
startServer :: IO ()
startServer = do
  putStrLn "Tax Calculator Server running on http://localhost:3000"
  scotty 3000 $ do
    -- Enable CORS for frontend
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "OPTIONS"]
      }
    
    -- Serve static files
    middleware $ staticPolicy (noDots >-> addBase "static")
    
    -- Health check endpoint
    get "/api/health" $ do
      json $ object ["status" .= ("ok" :: T.Text)]
    
    -- Calculate tax for single taxpayer
    post "/api/calculate" $ do
      req <- jsonData :: ActionM TaxRequest
      let response = processTaxRequest defaultBrackets req
      json response
    
    -- Calculate tax for batch of taxpayers
    post "/api/calculate-batch" $ do
      batchReq <- jsonData :: ActionM BatchRequest
      let requests = batchTaxpayers batchReq
          responses = map (processTaxRequest defaultBrackets) requests
          batchResp = BatchResponse responses
      json batchResp
    
    -- Get tax brackets info
    get "/api/brackets" $ do
      json $ map (\(TaxBracket lim rate) -> 
        object ["limit" .= lim, "rate" .= rate]) defaultBrackets
    
    -- Fallback route
    get "/" $ do
      file "static/index.html"