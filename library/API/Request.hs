{-# LANGUAGE DeriveGeneric  #-}

module API.Request (NewConsumption(..), consumptionToEntities) where

import Data.Aeson
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import qualified Storage.Model as Model

consumptionToEntities :: NewConsumption 
                      -> ( Model.Customer
                         , Model.Key Model.Customer -> Model.MeatBar
                         )
consumptionToEntities NewConsumption
  { eatenAt = mbEatenAt
  , meatBarType = mbType
  , customerName = name
  } =
  ( Model.Customer { Model.customerName = name }
  , \identity -> Model.MeatBar
      { Model.meatBarCustomer = identity
      , Model.meatBarType = mbType
      , Model.meatBarEatenAt = mbEatenAt
      }
  )

-- | The request type for the /consumptions route
-- 
-- Requires JSON of form:
-- 
-- { customerName: string
-- , meatBarType: string
-- , eatenAt: "2015-01-10T02:00:00.000Z"
-- }
data NewConsumption = NewConsumption
  { eatenAt :: UTCTime
  , meatBarType :: String
  , customerName :: String
  } deriving (Eq, Generic)

instance FromJSON NewConsumption