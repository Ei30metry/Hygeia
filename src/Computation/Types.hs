{-# LANGUAGE TupleSections #-}
-- |
module Computation.Types where

import           Computation.Utils

import           Control.Monad     ( foldM, join, (<=<), (=<<) )

import           Data.List         ( groupBy, sort )
import           Data.Ratio
import           Data.Time         ( Day, DiffTime, defaultTimeLocale,
                                     formatTime, secondsToDiffTime )
import           Data.Vector       ( Vector )
import qualified Data.Vector       as V

import           Text.Read         ( readEither )


data Mood = Angry Intensity
          | Sad Intensity
          | Neutral
          | Happy Intensity
          | Excited Intensity
          deriving (Read, Eq, Ord, Show)


newtype Moods = Moods (Vector Mood) deriving (Eq, Ord, Show)

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


instance Semigroup Intensity where
  x <> y = computeIntensity x y


instance Monoid Intensity where
  mappend = (<>)
  mempty = None


sameMood :: Mood -> Mood -> Bool
sameMood (Angry _) (Angry _)     = True
sameMood (Sad _) (Sad _)         = True
sameMood Neutral Neutral         = True
sameMood (Happy _) (Happy _)     = True
sameMood (Excited _) (Excited _) = True
sameMood _ _                     = False

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


combineMoodList :: [Mood] -> Maybe [Mood]
combineMoodList = traverse foldMoods . groupBy sameMood . sort
  where
    foldMoods ms = head' ms >>= \m -> foldM combineMoods m ms -- don't use head


type Name = String


data Alcohol = Alcohol { drink :: String
                       , shots :: Int } deriving (Eq, Ord, Show)

newtype Drinks = Drinks (Vector Alcohol) deriving (Show, Eq)

data Sleep = SP { wakeUpTime :: DiffTime
                , sleepTime  :: DiffTime } deriving (Eq, Ord)

instance Show Sleep where
  show (SP w s) = mconcat ["wake up: ",formatTime defaultTimeLocale "%H:%M" w,"\n"
                          ,"Sleep: ",formatTime defaultTimeLocale "%H:%M" s]


newtype Meditation = Med { unMed :: String } deriving (Eq, Ord)
newtype Meditations = Meds (Vector Meditation, DiffTime) deriving (Eq, Show)


instance Semigroup Meditations where
  (Meds (a,s1)) <> (Meds (b,s2)) = Meds (a <> b, s1 + s2)

instance Monoid Meditations where
  mempty = Meds (V.empty, 0)
  mappend = (<>)


mkMeditaitons :: Vector Meditation -> Either String Meditations
mkMeditaitons meds =
  Meds . (meds,) . V.sum
    <$>
    mapM (return . secondsToDiffTime . (60*) <=< readEither @Integer . unMed) meds


newtype Productivity = Pro { unPro :: Rational } deriving (Eq, Ord)

instance Show Productivity where
  show (Pro a) = mconcat [show (numerator a) ,"/",show (denominator a)]

instance Semigroup Productivity where
  (Pro a) <> (Pro b) = Pro $ a + b

instance Monoid Productivity where
  mappend = (<>)
  mempty = Pro 0


instance Show Meditation where
  show (Med a) = show a


data Cigarette = Cigarette { cigaretteName :: String
                           , number        :: Double
                           , nicotine      :: Double
                           , tar           :: Double }
                           deriving (Eq, Ord, Show)


data Entry = Entry { entryDay          :: Day
                   , entryMoods        :: Moods
                   , entrySleep        :: Sleep
                   , entryProductivity :: Productivity
                   , entryMeditations  :: Meditations
                   , entryDrinks       :: Drinks
                   , entryCigarette    :: Cigarette
                   , entryRating       :: Rating }
            deriving (Eq, Show)


type Entries = Vector Entry
