{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Computation where

import           Data.Kind      (Constraint, Type)
import           Data.List
import           Data.Monoid
import           Data.Semigroup

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


type Minutes = Int

-- GADT representation of our MoodReport
data MoodReport (a :: Mood) (b :: Intensity) where
   Report :: forall a b. Mood -> Intensity -> MoodReport a b


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


-- mean or average of a list
mean :: (Real a, Fractional a) => [a] -> a
mean ns = total/count
  where total = (realToFrac  . sum) ns
        count = (realToFrac . length) ns

-- median of a list
median :: (Ord a, Fractional a, Show a) => [a] -> a
median ns | odd n = sort ns !! (n `div` 2)
          | otherwise = ((sort ns !! (n `div` 2 - 1)) + (sort ns !! (n `div` 2))) / 2
        where n = length ns


rangeData :: (Num a, Ord a) => [a] -> a
rangeData ns = maximum ns - minimum ns


variance :: (Real a, Fractional a) => [a] -> a
variance ns = sum (map (\x -> (x - mean ns)^2) ns) / (fromIntegral . length) ns

--centralDeviation :: (Ord a, Fractional a, Show a, Floating a, Num a, Integral a) => [a] -> a
--centralDeviation :: (Ord a, Show a, RealFloat a, Integral a) => [a] -> a
centralDeviation :: (Real a, RealFloat a) => [a] -> a
centralDeviation = sqrt . variance


-- overallMood :: [MoodReport] -> Mood
-- overallMood = undefined


someFunc :: IO ()
someFunc = putStrLn "building ..."
