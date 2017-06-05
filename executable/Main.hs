{-# LANGUAGE OverloadedStrings #-}

import qualified API
import qualified Configuration
import qualified Storage


main :: IO ()
main =
  let 
    config = Configuration.hardcoded
  in do
    Storage.initialize config
    API.serve config