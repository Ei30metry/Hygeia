module Computation where

import qualified Data.Time                 as TI

-- import           Parser.Input
import           Control.Monad.Trans.Reader


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
newtype MoodReport = MR (Mood, Intensity) deriving (Show, Eq, Ord)


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
            | Great deriving (Show, Eq, Ord, Enum, Read, Bounded)


-- safe function to add MoodReports
addMoodReports :: MoodReport -> MoodReport -> Maybe MoodReport
addMoodReports = undefined
-- addMoodReports (FromSing l@(SMR (STuple2 a b))) (FromSing r@(SMR (STuple2 a' b'))) = do
--   Refl <- testEquality a a'
--   return $ FromSing $ addSingMoodReports l r


unsafeAddMoodReports :: MoodReport -> MoodReport -> MoodReport
unsafeAddMoodReports = undefined
-- unsafeAddMoodReports (FromSing l@(SMR (STuple2 a b))) (FromSing r@(SMR (STuple2 a' b')))
--   = case testEquality a a' of
--       Just Refl -> FromSing $ addSingMoodReports l r
--       _         -> undefined


unsafeCombineMRList :: [MoodReport] -> [MoodReport]
unsafeCombineMRList [] = []
unsafeCombineMRList [a] = [a]
unsafeCombineMRList moods@( MR x: MR x' : xs)
  | fst x == fst x' = (MR x) `unsafeAddMoodReports` (MR x') : unsafeCombineMRList xs
  | otherwise = MR x : MR x' : unsafeCombineMRList xs



type Name = String

data Alcohol = Alcohol { drink :: String
                       , shots :: Int } deriving (Eq, Ord)

data Sleep = SP { wakeUpTime :: TI.DiffTime
                , sleepTime  :: TI.DiffTime } deriving (Eq, Ord)



newtype Meditation = Med [String] deriving (Eq, Ord)

newtype Productivity = Pro (Int,Int) deriving (Eq, Ord)


instance Show Meditation where
  show (Med a) = show a


instance Show Productivity where
  show (Pro a) = show a


data Cigarette = Cigarette { number   :: Double
                           , nitocone :: Double
                           , tar      :: Double } deriving (Eq, Ord)


-- the prefix of E stand for entry
data EntryData = EName Name
               | EDay TI.Day
               | EMoodS [MoodReport]
               | ESleep Sleep
               | EProductivity Productivity
               | EMeditation Meditation
               | EAlcohol Alcohol
               | ECigarette Cigarette
               | ERating Rating
         deriving (Eq, Ord)

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
