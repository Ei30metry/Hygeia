{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NoStarIsType             #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}



module Computation where

import           Config
import           Data.Coerce
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
import           Data.Type.Equality
import           GHC.TypeLits
import           Parser


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
                               | Extreme deriving (Show, Read, Eq, Ord, Enum) |]

-- I'm using a newtype wrapper to be able to coerce it with the normal tuple when computing the data that comes from files
singletons [d| newtype MoodReport = MR (Mood, Intensity) deriving Show |]
--singletons [d| type MoodReport = (Mood, Intensity) |]


type family FromEnum' (a :: Intensity) :: Nat where
  FromEnum' Low = 1
  FromEnum' Medium = 2
  FromEnum' High = 3
  FromEnum' Extreme = 4

-- compute the bigger Intensity

-- return the ordering of two different Intensities
type family ReturnOrdering (a :: Intensity) (b :: Intensity) :: Ordering where
  ReturnOrdering a b = CmpNat (FromEnum' a) (FromEnum' b)


type family ReturnBiggerOne (a :: Intensity) (b :: Intensity) (c :: Ordering) :: Intensity where
  ReturnBiggerOne a b LT = b
  ReturnBiggerOne a b GT = a
  ReturnBiggerOne a b EQ = a


type family (a :: Intensity) <+> (b :: Intensity) :: Intensity where
  a <+> b = ReturnBiggerOne a b (ReturnOrdering a b)


-- instance SSemigroup Intensity where
--   (%<>) = toSing . (<>)


computeIntensity :: Intensity -> Intensity -> Intensity
computeIntensity x y | fromEnum x == fromEnum y = x
                     | fromEnum x > fromEnum y = x
                     | otherwise = y


-- promote [d| computeIntensity :: Intensity -> Intensity -> Intensity
--             computeIntensity x y | fromEnum x == fromEnum y = x
--                                  | fromEnum x > fromEnum y = x
--                                  | otherwise = y |]

instance Semigroup Intensity where
  x <> y = computeIntensity x y

-- singletons [d| instance Semigroup Intensity where
--                  x <> y = computeIntensity x y |]



instance Monoid Intensity where
  mappend = (<>)
  mempty = None

-- singletons [d| instance Monoid Intensity where
--                 mappend = (<>)
--                 mempty = None |]

data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great deriving (Show, Eq, Ord, Enum)



-- should be called with  TypeApplications
--example: moodReportToSing @Happy @Low ===> SMoodReport ('MR '(Happy,Low))
moodReportToSing :: forall (a :: Mood) (b :: Intensity). (SingI a, SingI b) => MoodReport -> SMoodReport ('MR '(a,b))
moodReportToSing (MR (a,b)) = sing :: Sing ('MR '(a, b))


-- addSingMoodReports :: forall a a' (b :: Intensity) (c :: Intensity) (d :: Intensity). SMoodReport ('MR '(a,b)) -> SMoodReport ('MR '(a, c)) -> SMoodReport ('MR '(a, b <+> c))
-- addSingMoodReports (SMR (STuple2 a b)) (SMR (STuple2 a' c)) = SMR $ STuple2 a (b  c)

unsafeAddMoodReports :: MoodReport -> MoodReport -> MoodReport
unsafeAddMoodReports (MR (a, c)) (MR (b, d)) = MR (a, d <> c)


-- not the actual term-level code of the code but just a representation of the
-- needed compositions
-- addMoodReports :: MoodReport -> MoodReport -> MoodReport
-- addMoodReports = unsafeAddMoodReports  . fromSing . addSingMoodReports . mooodReportToSing

someFunc :: IO ()
someFunc = putStrLn "building ..."
