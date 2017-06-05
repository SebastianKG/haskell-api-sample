module Storage.Import.CSV 
  ( CSVTime(..)
  , decodeMeatConsumptionCSV
  ) where

import Control.Applicative (empty)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Csv
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- Newtype wrapper to avoid orphaned instance.
newtype CSVTime = CSVTime UTCTime

instance FromField CSVTime where
  parseField bytes =
    case parseISO8601 $ UTF8.toString bytes of
      Just time -> pure $ CSVTime time
      Nothing -> empty

{-
  This function decodes a very specific type of CSV, of form:

  person,meat-bar-type,date        
    (where date is represented in the ISO 8601 date/time standard)

  The CSV must also have a header. It will be thrown away, 
  but it's expected to be there.
-}
decodeMeatConsumptionCSV :: ByteString 
                      -> Either String [(String, String, CSVTime)]
decodeMeatConsumptionCSV csv =
  let
    decoded :: Either String (Vector (String, String, CSVTime))
    decoded = decode HasHeader csv
  in
    Vector.toList <$> decoded