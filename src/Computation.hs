{-# LANGUAGE PolyKinds #-}

module Computation where



import Data.Semigroup.Singletons
    ( PSemigroup(type (<>)), SSemigroup((%<>)) )
import           Data.Singletons
import Data.Singletons.Base.Enum
    ( PBounded(..),
      PEnum(FromEnum, ToEnum),
      SBounded(..),
      FromEnumSym0,
      SEnum(sFromEnum, sToEnum) )
import           Data.Singletons.Base.TH
import qualified Data.Time                 as TI
import           Data.Type.Equality        ( TestEquality (testEquality) )

import           GHC.Base                  ( Double )

import Parser.Input


-- a Type representing one's mood with it's singleton definitions
singletons [d| data Mood = Angry
                         | Sad
                         | Neutral
                         | Happy
                         | Excited deriving (Read, Eq, Ord, Show) |]

-- Intensity of a mood
-- The None is not supposed to be used in a MoodReport but it is only here to
-- serve as a mempty for our Monoid instance
singletons [d| data Intensity = None
                              | Low
                              | Medium
                              | High
                              | Extreme deriving (Show, Read, Eq, Ord, Enum, Bounded) |]

-- using newtype to be able to coerce
singletons [d| newtype MoodReport = MR (Mood, Intensity) deriving (Show, Eq, Ord)|]


singletons [d| computeIntensity :: Intensity -> Intensity -> Intensity
               computeIntensity x y | fromEnum x == fromEnum y = x
                                    | fromEnum x > fromEnum y = x
                                    | otherwise = y |]


singletons [d| instance Semigroup Intensity where
                 x <> y = computeIntensity x y |]


instance Monoid Intensity where
  mappend = (<>)
  mempty = None


-- Rating data type for rating the day
data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great deriving (Show, Eq, Ord, Enum, Read,Bounded)


-- this function will only work if our SMoods are the same
addSingMoodReports :: forall a a' (b :: Intensity) (c :: Intensity). SMoodReport ('MR '(a, b)) -> SMoodReport ('MR '(a, c)) -> SMoodReport ('MR '(a, b <> c))
addSingMoodReports (SMR (STuple2 a b)) (SMR (STuple2 a' c)) = SMR $ STuple2 a (b %<> c)


-- safe function to add MoodReports
addMoodReports :: MoodReport -> MoodReport -> Maybe MoodReport
addMoodReports (FromSing l@(SMR (STuple2 a b))) (FromSing r@(SMR (STuple2 a' b'))) = do
  Refl <- testEquality a a'
  return $ FromSing $ addSingMoodReports l r


addMoodReports' :: MoodReport -> MoodReport -> MoodReport
addMoodReports' (FromSing l@(SMR (STuple2 a b))) (FromSing r@(SMR (STuple2 a' b')))
  = case testEquality a a' of
      Just Refl -> FromSing $ addSingMoodReports l r
      _         -> undefined


instance Semigroup MoodReport where
  (<>) = addMoodReports'


-- instance Monoid MoodReport where
--   mappend = (<>)
--   mempty = MR (Neutral, None)


type Name = String

type HeaderToComp b = forall a. (a ~ String ) => Header a -> b

data Alcohol = Alcohol { drink :: String
                       , shots :: Int } deriving (Eq, Ord)

data Sleep = SP { wakeUpTime :: TI.DiffTime
                , sleepTime  :: TI.DiffTime } deriving (Eq, Ord)

-- instance Show Sleep where
--   show (Sleep (H a, M b)) | a < 10 && b < 10 = mconcat ["0",show a,":", "0",show b]
--                           | a < 10 = mconcat ["0",show a,":",show b]
--                           | b < 10 = mconcat [show a,":","0",show b]
--                           | otherwise = mconcat [show a,":",show b]

newtype Meditation = Med [String] deriving (Eq, Ord)

newtype Productivity = Pro (Int,Int) deriving (Eq, Ord)


instance Show Meditation where
  show (Med a) = show a


instance Show Productivity where
  show (Pro a) = show a


data Cigarette = Cigarette { number   :: Double
                           , nitocone :: Double
                           , tar      :: Double } deriving (Eq, Ord)


data EntryData = EName Name
               | EDate TI.Day
               | EMoodS [MoodReport]
               | ESleep Sleep
               | EProductivity Productivity
               | EMeditation Meditation
               | EAlcohol Alcohol
               | ECigarette Cigarette
               | ERating Rating
         deriving (Eq, Ord)

-- parses the Name header type into the Name data type in order to compute

headerToEData :: forall a. (a ~ String) => (Header a) -> EntryData
headerToEData (NameH a) = EName a
headerToEData (DateH a) = undefined
headerToEData (MoodH a) = undefined
headerToEData (SleepH a) = undefined
headerToEData (ProductivityH a) = undefined
headerToEData (MeditationH a) = undefined
headerToEData (AlcoholH a) = undefined
headerToEData (CigaretteH a) = undefined
headerToEData (RatingH a) = undefined
headerToEData (AllHeaders a) = undefined

-- the prefix of E stand for entry


-- data DayReport = DR { name :: EntryData
--                     , date :: EntryData
