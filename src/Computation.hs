{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Computation where

import           Data.Coerce
import           Data.List
import           Parser

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

newtype MoodReport = MR (Mood, Intensity)

instance (a ~ Mood, b ~ Intensity) => Show MoodReport where
  show (MR (a, b)) = show a ++ " : " ++ show b



data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great deriving (Show, Eq, Ord, Enum)


data Header = MoodH
            | Name
            | Date
            | Sleep
            | Productivity
            | Rating
            | Meditation deriving Eq


-- instance Read Header where

-- instance (a ~ Header) => Show a where
--   show a = "[" ++ show a ++ "]"

instance Show Header where
  show Name = "Artin Ghasivand\n\n"
  show Date = "Date : "
  show MoodH         = "[Mood]\n\n"
  show Productivity = "[Productivity]\n\n"
  show Sleep = mconcat ["[Sleep]", "\n", "\n", "wake up :", "\n", "sleep :", "\n", "\n"]
  show Rating = "[Rating]\n"
  show Meditation = "[Meditation]\n\n"


type Minutes = Int



-- using newtype to be able to Coerce between this and the normal tuple

-- someFunc :: IO ()
-- someFunc = putStrLn "building ..."
