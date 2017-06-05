{-# LANGUAGE NamedFieldPuns #-}

module Analysis (popularDayForMonths, streaks) where

import Data.Traversable (sequence)
import Data.Function ((&))
import Data.List (group, groupBy, sort)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Time 
  ( Day
  , UTCTime(..)
  , fromGregorian
  , toGregorian
  , utctDay
  )
import Safe.Foldable (maximumByMay)

import API.Response hiding (Day)

streaks :: [UTCTime] -> [ConsumptionStreak]
streaks times =
  let
    consumptionDays =
      utctDay <$> times
  in
    consumptionDays
      & sort
      & group
      & fmap countAndFlatten
      & catMaybes
      & findStreaks
      & reverse
      & filter (not . oneDayStreak)

findStreaks :: [(Day, Int)] -> [ConsumptionStreak]
findStreaks countedDays =
  findStreaksHelper countedDays Nothing 0 []

data DayStreak = DayStreak
  { from :: Day
  , to :: Day
  }

dayStreakToConsumptionStreak :: DayStreak -> ConsumptionStreak
dayStreakToConsumptionStreak DayStreak{from, to} =
  let
    fromUTC = UTCTime
      { utctDay = from
      , utctDayTime = 0
      }
    
    toUTC = UTCTime
      { utctDay = to
      , utctDayTime = 0
      }
  in
    consumptionStreak (fromUTC, toUTC)

findStreaksHelper :: [(Day, Int)]
                  -> Maybe DayStreak
                  -> Int
                  -> [ConsumptionStreak] 
                  -> [ConsumptionStreak]

-- Base cases.
findStreaksHelper [] Nothing       _ acc = acc
findStreaksHelper [] (Just streak) _ acc =
  dayStreakToConsumptionStreak streak : acc

-- Initialize the streak to the current day.
findStreaksHelper ((day, score):list) Nothing _ acc =
  findStreaksHelper list (Just $ DayStreak day day) score acc

-- Keep storing days if the score is higher than before
-- otherwise, save the existing streak and start a new one with
-- the current day.
findStreaksHelper ((day, score): list) (Just streak) highScore acc =
  if score > highScore then
    findStreaksHelper list (Just $ streak{to = day}) score acc
  else
    findStreaksHelper 
      list 
      (Just $ DayStreak day day) 
      score
      (dayStreakToConsumptionStreak streak : acc)

oneDayStreak :: ConsumptionStreak -> Bool
oneDayStreak streak =
  consumptionStreakFrom streak == consumptionStreakTo streak

popularDayForMonths :: [UTCTime] -> [PopularDay] 
popularDayForMonths times =
  let
    wrapInResponse :: Day -> PopularDay
    wrapInResponse day =
      let
        (yearDigit, monthDigit, _) = toGregorian day
        firstDayOfMonth = fromGregorian yearDigit monthDigit 0

        dayUTC = UTCTime day 0
        monthUTC = UTCTime firstDayOfMonth 0
      in
        popularDay (monthUTC, dayUTC)
  in
    times
      & fmap utctDay
      & sort
      & groupBy (sameMonth)
      & fmap mostPopularDay
      & catMaybes
      & fmap wrapInResponse

sameMonth :: Day -> Day -> Bool
sameMonth day1 day2 =
  let
    (year1, month1, _) = toGregorian day1
    (year2, month2, _) = toGregorian day2
  in
    (year1, month1) == (year2, month2)

mostPopularDay :: [Day] -> Maybe Day
mostPopularDay days =
  let
    countedDays :: [Maybe (Day, Int)]
    countedDays = days
      & group
      & fmap countAndFlatten
  in do
    countTuples <- sequence countedDays
    mostPopular <- maximumByMay (comparing snd) countTuples
    return $ fst mostPopular

countAndFlatten :: [Day] -> Maybe (Day, Int)
countAndFlatten [] = Nothing
countAndFlatten dayGroup@(x:_) =
  Just (x, length dayGroup)