{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoStarIsType               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}


module Computation where


import           Control.Monad
import           Control.Monad.Trans
import           Data.Coerce
import           Data.Functor
import           Data.Kind                         (Constraint, Type)
import           Data.List
import           Data.Monoid
import           Data.Proxy
import           Data.Semigroup
import           Data.Singletons.Decide
import           Data.Singletons.Prelude.Enum
import           Data.Singletons.Prelude.Monoid
import           Data.Singletons.Prelude.Semigroup
import           Data.Singletons.TH
import qualified Data.Time                         as T
import           Data.Type.Equality                (TestEquality (testEquality))
import           GHC.Base                          (Double)
import qualified Parser                            as P


-- a Type representing one's mood with it's singleton definitions
singletons [d| data Mood = Angry
                         | Sad
                         | Neutral
                         | Happy
                         | Excited deriving (Read, Eq, Ord, Show) |]

-- Intensity of a mood
-- The None is not supposed to be used in a MoodReport but it is only here to
-- serve as an mempty for our Monoid instance
singletons [d| data Intensity = None
                              | Low
                              | Medium
                              | High
                              | Extreme deriving (Show, Read, Eq, Ord, Enum,Bounded) |]

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


newtype Sleep = Sleep (Hour,Minute) deriving Read


instance Show Minute where
  show (M a) = show a


instance Show Hour where
  show (H a) = show a


data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }


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



type Meditation = [String]

type Productivity = (Int,Int)

data Cigarette = Cigarette { number   :: Double
                           , nitocone :: Double
                           , tar      :: Double } deriving Eq


nameHtoName :: HeaderToComp Name
nameHtoName (P.Name a) = a :: Name


dateHtoTime :: HeaderToComp Date
dateHtoTime = undefined


moodHtoMoodReport' :: (String,String) -> MoodReport
moodHtoMoodReport' (a,b) = MR (read a :: Mood, read b :: Intensity)


moodHtoMoodReport :: HeaderToComp [MoodReport]
moodHtoMoodReport (P.MoodH a) = map moodHtoMoodReport' a


sleepHtoSleep :: HeaderToComp Sleep
--sleepHtoSleep (P.Sleep (a,b)) = Sleep (H (read a :: Hour), M (read b :: Minute))
sleepHtoSleep (P.Sleep (a,b)) = Sleep (H $ read a :: Hour, M $ read b :: Minute)


-- mediatationHtoMediation' :: HeaderToComp Meditation
-- mediatationHtoMediation'


mediatationHtoMediation :: HeaderToComp Meditation
mediatationHtoMediation = undefined


productivityHtoProductivity :: HeaderToComp Productivity
productivityHtoProductivity (P.Productivity (a,b)) = (read a, read b)


alcoholHtoAlcohol :: HeaderToComp Alcohol
alcoholHtoAlcohol (P.Alcohol (a,b)) = Alcohol (read a) (read b)


cigaretteHtoCigratte :: HeaderToComp Cigarette
cigaretteHtoCigratte (P.Cigarette (a,b,c)) = Cigarette (read a) (read b) (read c)


ratingHtoRating :: HeaderToComp Rating
ratingHtoRating (P.Rating a) = read a :: Rating


someFunc :: IO ()
someFunc = putStrLn "building ..."
