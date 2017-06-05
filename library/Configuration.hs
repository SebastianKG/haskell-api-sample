{-# LANGUAGE OverloadedStrings #-}

module Configuration (Config(..), hardcoded) where

import Data.Text (Text)

data Config = Config
  { csvFileLocation :: Text
  , databaseName :: Text
  }

hardcoded :: Config
hardcoded = Config
  { csvFileLocation = "data.csv"
  , databaseName = "sqlite.db"
  }