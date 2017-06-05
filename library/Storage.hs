{-# LANGUAGE ScopedTypeVariables #-}

module Storage (connectionInfo, initialize) where

import Control.Exception (SomeException, catch)
import Control.Lens.Setter (set)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist
import Database.Persist.Sqlite

import Configuration (Config)
import qualified Configuration
import Storage.Import.CSV (CSVTime(..), decodeMeatConsumptionCSV)
import qualified Storage.Model as Model

-- | A custom SQLite connection type, where we enabled foreign keys.
connectionInfo :: Text -> SqliteConnectionInfo
connectionInfo databaseName =
  set fkEnabled True $ mkSqliteConnectionInfo databaseName

-- | Initialize the SQLite DB. This will migrate all tables.
initialize :: Config -> IO ()
initialize config = do
  meatConsumptions <-
    readMeatConsumptions (Configuration.csvFileLocation config)
  migrateAndPopulateDB config meatConsumptions

readMeatConsumptions :: Text -> IO [(String, String, CSVTime)]
readMeatConsumptions csvFile = do
  csv <- (ByteString.readFile $ Text.unpack csvFile) `catch` \(e :: SomeException) -> do
    print e
    return mempty

  case decodeMeatConsumptionCSV csv of
    Right meatConsumptions -> 
      return meatConsumptions
    
    Left errorMessage -> do
      putStrLn $ "Couldn't parse your CSV, for this reason: " ++ errorMessage
      return []

migrateAndPopulateDB :: Config -> [(String, String, CSVTime)] -> IO ()
migrateAndPopulateDB config meatConsumpstions =
  runSqliteInfo (connectionInfo $ Configuration.databaseName config) $ do
      runMigration Model.migrateAll
      forM_ meatConsumpstions $ \(customerName, meatBarType, CSVTime date) -> do
        customerId <- insert $ Model.Customer customerName
        insert_ $ Model.MeatBar customerId meatBarType date