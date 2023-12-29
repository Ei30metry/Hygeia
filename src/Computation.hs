module Computation (module Computation.Util
                   ,module Computation.Types
                   ,module Computation.Monad) where

import           Computation.Monad
import           Computation.Types
import           Computation.Util

import           Control.Monad              ( foldM, join, (<=<) )
import           Control.Monad.Trans.Reader

import           Data.Coerce
import           Data.List                  ( concat, groupBy, sort, sortOn )
import           Data.Time                  ( Day, DiffTime, defaultTimeLocale,
                                              formatTime )
import           Data.Vector                ( Vector )

import           GHC.Generics


-- a Type representing one's mood with it's singleton definitions
data Mood = Angry Intensity
          | Sad Intensity
          | Neutral
          | Happy Intensity
          | Excited Intensity
          deriving (Read, Eq, Ord, Show)


-- Intensity of a mood
-- The None is not supposed to be used in a MoodReport but it is only here to
-- serve as a mempty for our Monoid instance
data Intensity = None
               | Low
               | Medium
               | High
               | Extreme
               deriving (Show, Read, Eq, Ord, Enum, Bounded)

computeIntensity :: Intensity -> Intensity -> Intensity
computeIntensity x y | fromEnum x == fromEnum y = x
                     | fromEnum x > fromEnum y = x
                     | otherwise = y

sameMood :: Mood -> Mood -> Bool
sameMood (Angry _) (Angry _)     = True
sameMood (Sad _) (Sad _)         = True
sameMood Neutral Neutral         = True
sameMood (Happy _) (Happy _)     = True
sameMood (Excited _) (Excited _) = True
sameMood _ _                     = False

instance Semigroup Intensity where
  x <> y = computeIntensity x y


instance Monoid Intensity where
  mappend = (<>)
  mempty = None


-- Rating data type for rating the day
data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great
            deriving (Show, Eq, Ord, Enum, Read, Bounded)


-- safe function to add MoodReports
combineMoods :: Mood -> Mood -> Maybe Mood
combineMoods (Angry x) (Angry y)     = Just $ Angry (x <> y)
combineMoods (Sad x) (Sad y)         = Just $ Sad (x <> y)
combineMoods Neutral Neutral         = Just Neutral
combineMoods (Happy x) (Happy y)     = Just $ Happy (x <> y)
combineMoods (Excited x) (Excited y) = Just $ Excited (x <> y)
combineMoods _ _                     = Nothing


mrList = [Happy High, Sad Low, Happy Extreme
         ,Sad Extreme, Angry Medium, Neutral
         ,Neutral, Happy Extreme, Excited Low
         ,Sad High, Angry High] -- lift to Either


combineMoodList :: [Mood] -> Maybe [Mood]
combineMoodList = traverse foldMoods . groupBy sameMood . sort
  where
    foldMoods ms = head' ms >>= \m -> foldM combineMoods m ms -- don't use head

-- | Safe head
head' []     = Nothing
head' (x:xs) = Just x


type Name = String


data Alcohol = Alcohol { drink :: String
                       , shots :: Int } deriving (Eq, Ord, Show)


data Sleep = SP { wakeUpTime :: DiffTime
                , sleepTime  :: DiffTime } deriving (Eq, Ord)

instance Show Sleep where
  show (SP w s) = mconcat ["wake up: ",formatTime defaultTimeLocale "%H:%M" w,"\n"
                          ,"Sleep: ",formatTime defaultTimeLocale "%H:%M" s]

newtype Meditation = Med [String] deriving (Eq, Ord)


newtype Productivity = Pro (Int,Int) deriving (Eq, Ord)


instance Show Meditation where
  show (Med a) = show a


instance Show Productivity where
  show (Pro a) = show a


data Cigarette = Cigarette { number   :: Double
                           , nitocone :: Double
                           , tar      :: Double }
                           deriving (Eq, Ord, Show)


data Entry = Entry { entryDay          :: Day
                   , entryMoods        :: [Mood]
                   , entrySleep        :: Sleep
                   , entryProductivity :: Productivity
                   , entryMeditation   :: Meditation
                   , entryAlcohol      :: Alcohol
                   , entryCigarette    :: Cigarette
                   , entryRating       :: Rating }
            deriving (Eq, Ord, Show)


type Entries = Vector Entry
