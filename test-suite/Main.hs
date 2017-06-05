import Data.Time (UTCTime(..), fromGregorian)
import qualified Test.Tasty
import Test.Tasty.Hspec

import qualified Analysis
import qualified API.Response

main :: IO ()
main = do
    test <- testSpec "frontrowed-interview" spec
    Test.Tasty.defaultMain test

utcTimeFromGregorian :: (Integer, Int, Int) -> UTCTime 
utcTimeFromGregorian (y, m, d) =
    UTCTime (fromGregorian y m d) 0

spec :: Spec
spec = parallel $ do
    it "should return an empty list" $ do
        Analysis.streaks [] `shouldBe` []
    it "should find no streaks" $ do
        Analysis.streaks
            ( utcTimeFromGregorian <$>
            [ (2016, 1, 1)
            , (2016, 1, 2)
            , (2016, 1, 3)
            ] )
            `shouldBe` 
            []
    it "should find one streak" $ do
        Analysis.streaks 
            ( utcTimeFromGregorian <$>
            [ (2016, 1, 1)
            , (2016, 2, 2)
            , (2016, 2, 2)
            , (2016, 2, 3)
            ] )
            `shouldBe` 
            [ API.Response.consumptionStreak 
                ( (utcTimeFromGregorian (2016, 1, 1))
                , (utcTimeFromGregorian (2016, 2, 2))
                )
            ]
    it "should find no popular days" $ do
        Analysis.popularDayForMonths [] `shouldBe` []
    
    it "should find one popular day" $ do
        Analysis.popularDayForMonths
            ( utcTimeFromGregorian <$>
            [ (2017, 8, 8) -- future food
            ] )
            `shouldBe`
            [ API.Response.popularDay
                ( utcTimeFromGregorian (2017, 8, 1)
                , utcTimeFromGregorian (2017, 8, 8)
                )
            ]
    
    it "should find two popular days for two different months, sorted ASC" $ do
        Analysis.popularDayForMonths
            ( utcTimeFromGregorian <$>
            [ (2017, 1, 2)
            , (2017, 1, 2)
            , (2017, 1, 7)
            , (2017, 1, 7)
            , (2017, 1, 7)
            , (2017, 1, 8)
            , (2012, 1, 1)
            , (2012, 1, 2)
            , (2012, 1, 1)
            , (2012, 1, 2)
            , (2012, 1, 1)
            ] )
            `shouldBe`
            [ API.Response.popularDay
                ( utcTimeFromGregorian (2012, 1, 1)
                , utcTimeFromGregorian (2012, 1, 1)
                )
            , API.Response.popularDay
                ( utcTimeFromGregorian (2017, 1, 1)
                , utcTimeFromGregorian (2017, 1, 7)
                )
            ]