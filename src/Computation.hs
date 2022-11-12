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

import qualified Parser.Input              as P


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

type HeaderToComp b = forall a. (a ~ String ) => P.Header a -> b

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


-- parses the Name header type into the Name data type in order to compute
nameHtoName :: HeaderToComp Name
nameHtoName (P.NameH a) = a :: Name

-- parses the Date header type into the Date data type in order to compute
-- dateHtoDate :: HeaderToComp Date
-- dateHtoDate (P.DateH (a,b,c)) = Date (Y $ read a) (Mo $ read b) (D $ read c)

-- helper function in order to convert tuple to MoodReport
moodHtoMoodReport' :: (String,String) -> MoodReport
moodHtoMoodReport' (a,b) = MR (read a, read b)

-- parses the Name header type into the Name data type in order to compute
moodHtoMoodReport :: HeaderToComp [MoodReport]
moodHtoMoodReport (P.MoodH a) = map moodHtoMoodReport' a

-- parses the Sleep header type into the Sleep data type in order to compute
-- sleepHtoSleep :: HeaderToComp Sleep
-- sleepHtoSleep (P.SleepH (a,b)) = Sleep (H $ read a, M $ read b )


-- parses the Meditation header type into the Meditation data type in order to compute
mediatationHtoMediation :: HeaderToComp Meditation
mediatationHtoMediation (P.MeditationH a) = Med a


-- parses the Productivity header type into the Productivity data type in order to compute
productivityHtoProductivity :: HeaderToComp Productivity
productivityHtoProductivity (P.ProductivityH (a,b)) = Pro (read a, read b)


-- parses the Alcohol header type into the Alcohol data type in order to compute
alcoholHtoAlcohol :: HeaderToComp Alcohol
alcoholHtoAlcohol (P.AlcoholH (a,b)) = Alcohol (read a) (read b)


-- parses the Cigarette header type into the Cigarette data type in order to compute
cigaretteHtoCigratte :: HeaderToComp Cigarette
cigaretteHtoCigratte (P.CigaretteH (a,b,c)) = Cigarette (read a) (read b) (read c)


-- parses the Rating header type into the Rating data type in order to compute
ratingHtoRating :: HeaderToComp Rating
ratingHtoRating (P.RatingH a) = read a :: Rating

-- the prefix of E stand for entry
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


-- data DayReport = DR { name :: EntryData
--                     , date :: EntryData
--                     , moods []}
