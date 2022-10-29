{-# LANGUAGE PolyKinds #-}

module Computation where


import           Control.Monad
import           Control.Monad.Trans

import           Data.Coerce
import           Data.Functor
import           Data.Kind                 ( Constraint, Type )
import           Data.List
import           Data.Monoid
import           Data.Proxy
import           Data.Semigroup
import           Data.Semigroup.Singletons
import           Data.Singletons
import           Data.Singletons.Base.Enum
import           Data.Singletons.Base.TH
import qualified Data.Time                 as TI
import           Data.Type.Equality        ( TestEquality (testEquality) )

import           GHC.Base                  ( Double )

import qualified Parser                    as P


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
singletons [d| newtype MoodReport = MR (Mood, Intensity) deriving Show |]


singletons [d| computeIntensity :: Intensity -> Intensity -> Intensity
               computeIntensity x y | fromEnum x == fromEnum y = x
                                    | fromEnum x > fromEnum y = x
                                    | otherwise = y |]


singletons [d| instance Semigroup Intensity where
                 x <> y = computeIntensity x y |]


-- monoid instance for Intensity
singletons [d| instance Monoid Intensity where
                 mappend = (<>)
                 mempty = None |]


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


-- for capturing everything after we convert the result of parseEntry into their corresponding types
data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:>) :: t -> HList ts -> HList (t ': ts)


-- parseResult :: 'HList [Name,Date,MoodH,Sleep,Productivity,Meditation,Alcohol,Cigarette,Rating]
-- parseResult = undefined


-- type synonyms for more descriptive type signatures
type Name = String

type HeaderToComp b = forall a. (a ~ String ) => P.Header a -> b

data Alcohol = Alcohol { drink :: String
                       , shots :: Int }

newtype Hour = H Int deriving (Read,Enum)

newtype Minute = M Int deriving (Read,Enum)

newtype Year = Y Int deriving (Read,Enum)

newtype Month = Mo Int deriving (Read,Enum)

newtype Day = D Int deriving (Read,Enum)


instance Bounded Hour where
  minBound = H 0
  maxBound = H 24


instance Bounded Minute where
  minBound = M 0
  maxBound = M 60


instance Show Year where
  show (Y a) = show a

instance Show Month where
  show (Mo a) = show a

instance Show Day where
  show (D a) = show a

newtype Sleep = Sleep (Hour,Minute) deriving Read


instance Show Minute where
  show (M a) = show a


instance Show Hour where
  show (H a) = show a


data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }

instance Show Date where
  show (Date a b c) = mconcat [show a,"-",show b, "-", show c]

-- type Date = TI.UTCTime

instance Bounded Month where
  minBound = Mo 1
  maxBound = Mo 12


instance Bounded Day where
  minBound = D 1
  maxBound = D 30


instance Show Sleep where
  show (Sleep (H a, M b)) | a < 10 && b < 10 = mconcat ["0",show a,":", "0",show b]
                          | a < 10 = mconcat ["0",show a,":",show b]
                          | b < 10 = mconcat [show a,":","0",show b]
                          | otherwise = mconcat [show a,":",show b]


-- using newtype instead of type in order to coerce and add a little bit of more
newtype Meditation = Med [String]

newtype Productivity = Pro (Int,Int)


instance Show Meditation where
  show (Med a) = show a


instance Show Productivity where
  show (Pro a) = show a


data Cigarette = Cigarette { number   :: Double
                           , nitocone :: Double
                           , tar      :: Double } deriving Eq


-- parses the Name header type into the Name data type in order to compute
nameHtoName :: HeaderToComp Name
nameHtoName (P.NameH a) = a :: Name

-- parses the Date header type into the Date data type in order to compute
dateHtoDate :: HeaderToComp Date
dateHtoDate (P.DateH (a,b,c)) = Date (Y $ read a) (Mo $ read b) (D $ read c)

-- helper function in order to convert tuple to MoodReport
moodHtoMoodReport' :: (String,String) -> MoodReport
moodHtoMoodReport' (a,b) = MR (read a, read b)

-- parses the Name header type into the Name data type in order to compute
moodHtoMoodReport :: HeaderToComp [MoodReport]
moodHtoMoodReport (P.MoodH a) = map moodHtoMoodReport' a

-- parses the Sleep header type into the Sleep data type in order to compute
sleepHtoSleep :: HeaderToComp Sleep
sleepHtoSleep (P.SleepH (a,b)) = Sleep (H $ read a, M $ read b )


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



someFunc :: IO ()
someFunc = putStrLn "building ..."
