{-# LANGUAGE PolyKinds #-}

module Computation where



import           Data.Semigroup.Singletons ( PSemigroup (type (<>)),
                                             SSemigroup ((%<>)) )
import           Data.Singletons
import           Data.Singletons.Base.Enum ( FromEnumSym0, PBounded (..),
                                             PEnum (FromEnum, ToEnum),
                                             SBounded (..),
                                             SEnum (sFromEnum, sToEnum) )
import           Data.Singletons.Base.TH
import qualified Data.Time                 as TI
import           Data.Type.Equality        ( TestEquality (testEquality) )

import           GHC.Base                  ( Double )

import           Parser.Input
import           Control.Monad.Trans.Reader


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


unsafeAddMoodReports :: MoodReport -> MoodReport -> MoodReport
unsafeAddMoodReports (FromSing l@(SMR (STuple2 a b))) (FromSing r@(SMR (STuple2 a' b')))
  = case testEquality a a' of
      Just Refl -> FromSing $ addSingMoodReports l r
      _         -> undefined

-- converts a tuple to UTCTime
tupleToUTCTime :: (String,String,String,String,String,String) -> TI.UTCTime
tupleToUTCTime (a,b,c,d,e,f) = MD.UTCTime day (MD.secondsToDiffTime seconds)
  where day = fromJulian (read @Integer a) (fromEnum (read @Months b)) (read @Int c)
        d' = (read d, read e) :: (Integer, Integer)
        seconds = (* 60) $ (60 * fst d') + snd d' + read @Integer f



-- (<!>) :: Maybe MoodReport -> Maybe MoodReport -> Maybe MoodReport
-- mr <!> mr' = addMoodReports <$> mr <*> mr'


-- the reason of it being unsafe is that
unsafeCombineMRList :: [MoodReport] -> [MoodReport]
unsafeCombineMRList [] = []
unsafeCombineMRList [a] = [a]
unsafeCombineMRList moods@( MR x: MR x' : xs)
  | fst x == fst x' = (MR x) `unsafeAddMoodReports` (MR x') : unsafeCombineMRList xs
  | otherwise = MR x : MR x' : unsafeCombineMRList xs



type Name = String

type HeaderToComp b = forall a. (a ~ String ) => Header a -> b

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
               | EDate TI.Day
               | EMoodS [MoodReport]
               | ESleep Sleep
               | EProductivity Productivity
               | EMeditation Meditation
               | EAlcohol Alcohol
               | ECigarette Cigarette
               | ERating Rating
         deriving (Eq, Ord)


-- converts the Name header type into the Name data type in order to compute

headerToEData :: forall a. (a ~ String) => (Header a) -> EntryData
headerToEData (NameH a)         = EName a
headerToEData (DateH a)         = undefined
headerToEData (MoodReportH a)         = undefined
headerToEData (SleepH a)        = undefined
headerToEData (ProductivityH a) = undefined
headerToEData (MeditationH a)   = undefined
headerToEData (AlcoholH a)      = undefined
headerToEData (CigaretteH a)    = undefined
headerToEData (RatingH a)       = undefined
-- headerToEData (AllHeaders a)    = undefined


getDate :: [Header a] -> [Header a]
getDate [] = []
getDate (DateH a: xs) = [DateH a]
getDate (x:xs) = getDate xs

entryToEData :: forall a. (a ~ String) => [Header a] -> [EntryData]
entryToEData = undefined
