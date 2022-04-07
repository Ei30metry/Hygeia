module Computation where

import           Data.List

-- a Type representing one's mood
data Mood = Angry
          | Sad
          | Neutral
          | Happy
          | Excited deriving (Read, Eq, Ord, Show)


data Intensity = Low
               | Medium
               | High
               | Extreme deriving (Show, Read, Eq, Ord)


-- Enum instance for computing moods together
instance Enum Mood where
  fromEnum Angry   = -2
  fromEnum Sad     = -1
  fromEnum Neutral = 0
  fromEnum Happy   = 1
  fromEnum Excited = 2
  toEnum (-2) = Angry
  toEnum (-1) = Sad
  toEnum 0    = Neutral
  toEnum 1    = Happy
  toEnum 2    = Excited

class ToFrac a where
  toFrac :: Fractional b => a -> b
  fracToType :: (Fractional b, Eq b) => b -> Maybe a

instance ToFrac Intensity where
  toFrac Low     = 0.25
  toFrac Medium  = 0.5
  toFrac High    = 0.75
  toFrac Extreme = 1
  fracToType 0.25 = Just Low
  fracToType 0.5  = Just Medium
  fracToType 0.75 = Just High
  fracToType 1    = Just Extreme
  fracToType n    = Nothing

mean :: (Real a, Fractional a) => [a] -> a
mean ns = total/count
  where total = (realToFrac  . sum) ns
        count = (realToFrac . length) ns


median :: (Ord a, Fractional a, Show a) => [a] -> a
median ns | odd n = sort ns !! (n `div` 2)
          | otherwise = ((sort ns !! (n `div` 2 - 1)) + (sort ns !! (n `div` 2))) / 2
        where n = length ns


rangeData :: (Num a, Ord a) => [a] -> a
rangeData ns = maximum ns - minimum ns


variance :: (Ord a, Fractional a, Show a) => [a] -> a
variance = undefined


someFunc :: IO ()
someFunc = putStrLn "building ..."


