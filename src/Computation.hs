module Computation where

import           Control.Monad              ( join, (<=<), foldM )
import           Control.Monad.Trans.Reader

import           Data.Coerce
import           Data.List                  ( groupBy, sortOn, concat, sort )
import           Data.Time                  ( Day, DiffTime )


-- a Type representing one's mood with it's singleton definitions
data Mood = Angry
          | Sad
          | Neutral
          | Happy
          | Excited deriving (Read, Eq, Ord, Show)

-- Intensity of a mood
-- The None is not supposed to be used in a MoodReport but it is only here to
-- serve as a mempty for our Monoid instance
data Intensity = None
               | Low
               | Medium
               | High
               | Extreme
               deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- using newtype to be able to coerce
-- get rid of this and combine it into Mood itself.
newtype MoodReport = MR { getMR :: (Mood, Intensity) }
   deriving (Eq, Ord)

instance Show MoodReport where
  show (MR (a,b)) = mconcat ["(" , show a, " , " ,show b, ")"]

computeIntensity :: Intensity -> Intensity -> Intensity
computeIntensity x y | fromEnum x == fromEnum y = x
                     | fromEnum x > fromEnum y = x
                     | otherwise = y


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
combineMoodReports :: MoodReport -> MoodReport -> Maybe MoodReport
combineMoodReports (MR (a,b)) (MR (c,d))
  | a == c = Just $ MR (a, b <> d)
  | otherwise = Nothing


mrList = map MR [(Happy,High), (Sad,Low), (Happy,Extreme), (Sad, Extreme), (Angry,Medium), (Neutral,Medium), (Neutral, High), (Happy,Extreme), (Excited,Low), (Sad, High)]

-- lift to Either
-- NOTE: write tests
combineMRList :: [MoodReport] -> Maybe [MoodReport]
combineMRList = traverse foldMoods . groupBy (\x y -> getMRMood x == getMRMood y) . sort
  where
    foldMoods mr = head' mr >>= \mrh -> foldM combineMoodReports mrh mr -- don't use head

-- | Safe head
head' [] = Nothing
head' (x:xs) = Just x


getMRMood = fst . getMR
getMRIntensity = snd . getMR


type Name = String


data Alcohol = Alcohol { drink :: String
                       , shots :: Int } deriving (Eq, Ord, Show)


data Sleep = SP { wakeUpTime :: DiffTime
                , sleepTime  :: DiffTime } deriving (Eq, Ord, Show)


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


data EntryData = EName Name
               | EDay Day
               | EMoodS [MoodReport]
               | ESleep Sleep
               | EProductivity Productivity
               | EMeditation Meditation
               | EAlcohol Alcohol
               | ECigarette Cigarette
               | ERating Rating
         deriving (Eq, Ord, Show)

-- converts the Name header type into the Name data type in order to compute
-- headerToEData :: forall a. Header a -> EntryData
-- headerToEData (NameH a)         = undefined
-- headerToEData (DateH a)         = undefined
-- headerToEData (MoodReportH a)         = undefined
-- headerToEData (SleepH a)        = undefined
-- headerToEData (ProductivityH a) = undefined
-- headerToEData (MeditationH a)   = undefined
-- headerToEData (AlcoholH a)      = undefined
-- headerToEData (CigaretteH a)    = undefined
-- headerToEData (RatingH a)       = undefined
-- headerToEData (AllHeaders a)    = undefined


-- entryToEData :: forall a. [Header a] -> [EntryData]
-- entryToEData = undefined
