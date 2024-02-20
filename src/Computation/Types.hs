{-# LANGUAGE TupleSections #-}
-- |
module Computation.Types where

import           Computation.Utils

import           Control.Monad     ( foldM, join, (<=<), (=<<) )

import           Data.Coerce
import           Data.Foldable     ( toList )
import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Data.Time         ( Day, DiffTime, defaultTimeLocale,
                                     formatTime, secondsToDiffTime )

import           Text.Read         ( readEither )


data Mood = Angry Intensity
          | Sad Intensity
          | Neutral
          | Happy Intensity
          | Excited Intensity
          deriving (Read, Eq, Ord, Show)


newtype Moods = Moods { unMoods :: [Mood] } deriving (Eq, Ord, Show)

-- | Intensity of a mood
data Intensity = None      -- Only here because of mempty
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


condenseMoods :: [Mood] -> [Mood]
condenseMoods = fmap go . groupBy sameMood . sort
  where
    x <!> y = fromJust (combineMoods x y)
    go xs
      | length xs == 1 = head xs
      | otherwise = foldr (<!>) (head xs) xs


type Name = String


data Alcohol = Alcohol { drink :: String
                       , shots :: Int } deriving (Eq, Ord, Show)

newtype Drinks = Drinks [Alcohol] deriving (Show, Eq)

data Sleep = SP { wakeUpTime :: DiffTime
                , sleepTime  :: DiffTime } deriving (Eq, Ord)

instance Show Sleep where
  show (SP w s) = mconcat ["wake up: ",formatTime defaultTimeLocale "%H:%M" w,"\n"
                          ,"Sleep: ",formatTime defaultTimeLocale "%H:%M" s]


newtype Meditation = Med { unMed :: String } deriving (Eq, Ord)

-- NOTE Remove DiffTime, and compute it during summarization
newtype Meditations = Meds ([Meditation], DiffTime) deriving (Eq, Show)


instance Semigroup Meditations where
  (Meds (a,s1)) <> (Meds (b,s2)) = Meds (a <> b, s1 + s2)


instance Monoid Meditations where
  mempty = Meds ([], 0)
  mappend = (<>)


mkMeditaitons :: [Meditation] -> Either String Meditations
mkMeditaitons meds =
  Meds . (meds,) . sum
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


data Stage = Parser | Summaraizer deriving (Show, Eq)  

type family SummaryType a | a -> a where
  SummaryType Moods        = (Moods,Moods)
  SummaryType Sleep        = ([Sleep],Sleep)
  SummaryType Drinks       = (Drinks,Drinks)
  SummaryType Rating       = ([Rating],Rating)
  SummaryType Productivity = ([Productivity],Productivity)
  SummaryType Cigarette    = ([Cigarette],[Cigarette])
  SummaryType Meditations  = ([Meditations],Meditation)


type family XXDay a where
  XXDay Parser      = Day
  XXDay Summaraizer = SummaryType Day

type family XXMoods a where
  XXMoods Parser      = Moods
  XXMoods Summaraizer = SummaryType Moods


type family XXProductivity a where
  XXProductivity Parser      = Productivity
  XXProductivity Summaraizer = SummaryType Productivity


type family XXMeditations a where
  XXMeditations Parser      = Meditations
  XXMeditations Summaraizer = SummaryType Meditations


type family XXSleep a where
  XXSleep Parser      = Sleep
  XXSleep Summaraizer = SummaryType Sleep


type family XXDrinks a where
  XXDrinks Parser      = Drinks
  XXDrinks Summaraizer = SummaryType Drinks


type family XXCigarette a where
  XXCigarette Parser      = Drinks
  XXCigarette Summaraizer = SummaryType Drinks


type family XXRating a where
  XXRating Parser = Rating
  XXRating Summaraizer = SummaryType Rating
  

data Entry a = Entry { entryDay          :: XXDay a
                     , entryMoods        :: XXMoods a 
                     , entrySleep        :: XXSleep a 
                     , entryProductivity :: XXProductivity a 
                     , entryMeditations  :: XXMeditations a 
                     , entryDrinks       :: XXDrinks a 
                     , entryCigarette    :: XXCigarette a
                     , entryRating       :: XXRating a }
