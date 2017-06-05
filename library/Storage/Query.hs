{-# LANGUAGE NamedFieldPuns #-}

module Storage.Query
  ( allCustomers
  , allConsumptions
  , consumptionStreaks
  , insertConsumption
  , popularDayForMonths
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Esqueleto
import qualified Database.Persist as SimpleQuery
import qualified Database.Persist.Sqlite as SQLite

import qualified Analysis
import qualified API.Response
import Configuration (Config(..))
import qualified Storage
import qualified Storage.Model as Model

transactionally :: Text 
                -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a 
                -> IO a
transactionally databaseName = 
  SQLite.runSqliteInfo (Storage.connectionInfo databaseName)

allCustomers :: ReaderT Config IO [API.Response.Customer]
allCustomers = do
  Config{databaseName} <- ask

  entities <-
    liftIO $ transactionally databaseName $ 
      SimpleQuery.selectList [] []

  let customerModels = entityVal <$> entities

  return $ API.Response.customer <$> customerModels

allConsumptions :: ReaderT Config IO [API.Response.Consumption]
allConsumptions = do
  Config{databaseName} <- ask

  consumptionTuples <- liftIO $ transactionally databaseName $
    select $
    from $ \(customer, meatBar) -> do
    where_ (customer ^. Model.CustomerId ==. meatBar ^. Model.MeatBarCustomer)
    return (customer, meatBar)
  let getValues (a, b) = (entityVal a, entityVal b)
      consumptionEntities = getValues <$> consumptionTuples 
  return $ API.Response.consumption <$> consumptionEntities

meatBarEatingTimes :: ReaderT Config IO [UTCTime]
meatBarEatingTimes = do
  Config{databaseName} <- ask

  values <- liftIO $ transactionally databaseName $
    select $
    from $ \(meatBar) -> do
    return (meatBar ^. Model.MeatBarEatenAt)

  return $ unValue <$> values

consumptionStreaks :: ReaderT Config IO [API.Response.ConsumptionStreak]
consumptionStreaks = do
  times <- meatBarEatingTimes
  return $ Analysis.streaks times

popularDayForMonths :: ReaderT Config IO [API.Response.PopularDay]
popularDayForMonths = do
  times <- meatBarEatingTimes
  return $ Analysis.popularDayForMonths times

insertConsumption :: Model.Customer
                  -> (Model.Key Model.Customer -> Model.MeatBar)
                  -> ReaderT Config IO ()
insertConsumption customer meatBarFunction = do
  Config{databaseName} <- ask
  liftIO $ transactionally databaseName $ do
    customerId <- insert customer
    insert_ $ meatBarFunction customerId