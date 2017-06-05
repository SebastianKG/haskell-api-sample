{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module API.Response
  ( Customer(..)
  , Consumption(..)
  , ConsumptionStreak(..)
  , Day
  , FromTime
  , Month
  , PopularDay(..)
  , ToTime
  , customer
  , consumption
  , consumptionStreak
  , popularDay
  ) where

import Data.Aeson
import Data.Aeson.Types (fieldLabelModifier)
import Data.Char (toLower)
import GHC.Generics (Generic)
import Data.Time (UTCTime)

import qualified Storage.Model as Model

customer :: Model.Customer -> Customer
customer Model.Customer{customerName = name} = Customer
  { customerName = name
  }

consumption :: (Model.Customer, Model.MeatBar) -> Consumption
consumption (thisCustomer, Model.MeatBar{..}) = Consumption
  { consumptionCustomer = customer thisCustomer
  , consumptionMeatBarType = meatBarType
  , consumptionMeatBarEatenAt = meatBarEatenAt
  }

type FromTime = UTCTime
type ToTime = UTCTime

consumptionStreak :: (FromTime, ToTime) -> ConsumptionStreak
consumptionStreak (from, to) = ConsumptionStreak
  { consumptionStreakFrom = from
  , consumptionStreakTo = to
  } 

type Month = UTCTime
type Day = UTCTime

popularDay :: (Month, Day) -> PopularDay
popularDay (month, day) = PopularDay
  { popularDate = day
  , popularForMonth = month
  }

-- | The response type for the /customers route
-- 
-- Generates JSON of form { name: string }
data Customer = Customer
  { customerName :: String
  } deriving (Eq, Generic)

-- | The response type for the /consumptions route
-- 
-- Generates JSON of form:
-- 
-- { customer: {name: string}
-- , meatBarType: string
-- , eatenAt: "2015-01-10T02:00:00.000Z"
-- }
data Consumption = Consumption
  { consumptionCustomer :: Customer
  , consumptionMeatBarType :: String
  , consumptionMeatBarEatenAt :: UTCTime
  } deriving (Eq, Generic)

-- | The response type for the /consumption-streaks route
-- 
-- Generates JSON of form:
-- 
-- { from: "2015-01-10T02:00:00.000Z"
-- , to: "2015-01-11T02:00:00.000Z"
-- }
data ConsumptionStreak = ConsumptionStreak
  { consumptionStreakFrom :: UTCTime
  , consumptionStreakTo :: UTCTime 
  } deriving (Eq, Generic, Show)

-- | The response type for the /most-popular-days-for-months route
-- 
-- Generates JSON of form:
-- 
-- { date: "2015-01-10T02:00:00.000Z"
-- , forMonth: "2015-01-01T02:00:00.000Z"
-- }
data PopularDay = PopularDay 
  { popularDate :: UTCTime
  , popularForMonth :: UTCTime
  } deriving (Eq, Generic, Show)

instance ToJSON Customer where
  toEncoding = genericToEncoding $ defaultOptions 
    { fieldLabelModifier = changeFirst toLower . drop 8 }

instance ToJSON Consumption where
  toEncoding = genericToEncoding $ defaultOptions 
    { fieldLabelModifier = changeFirst toLower . drop 11 }

instance ToJSON ConsumptionStreak where
  toEncoding = genericToEncoding $ defaultOptions 
    { fieldLabelModifier = changeFirst toLower . drop 17 }

instance ToJSON PopularDay where
  toEncoding = genericToEncoding $ defaultOptions 
    { fieldLabelModifier = changeFirst toLower . drop 7 }

changeFirst :: (a -> a) -> [a] -> [a]
changeFirst _ []     = []
changeFirst f (x:xs) = (f x):xs