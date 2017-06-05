{-# LANGUAGE OverloadedStrings  #-}

module API (serve) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans
  ( ScottyT
  , get
  , json
  , jsonData
  , post
  , scottyT
  )

import qualified API.Request
import Configuration (Config)
import qualified Storage.Query

-- | Setting up and serving the Meat API.
-- Response formats can be seen in API.Response
-- The request format for the POST route can be seen in API.Request.
serve :: Config -> IO ()
serve config =
  let
    configReader = \r -> runReaderT r config
  in
    scottyT 3000 configReader routes

routes :: ScottyT Text (ReaderT Config IO) ()
routes = do
  get "/customers" $ do
    customers <- lift $ Storage.Query.allCustomers
    json customers

  get "/consumptions" $ do
    consumptions <- lift $ Storage.Query.allConsumptions
    json consumptions

  post "/consumptions" $ do
    newConsumption <- jsonData
    let (customer, meatBarFunction) = 
          API.Request.consumptionToEntities newConsumption
    lift $ Storage.Query.insertConsumption customer meatBarFunction
  
  get "/consumption-streaks" $ do
    consumptionStreaks <- lift $ Storage.Query.consumptionStreaks
    json consumptionStreaks
  
  get "/most-popular-days-for-months" $ do
    popularDays <-
      lift $ Storage.Query.popularDayForMonths
    json popularDays