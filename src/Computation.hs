{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NoStarIsType             #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}



module Computation where

import           Config
import           Data.Coerce
import           Data.Kind                (Constraint, Type)
import           Data.List
import           Data.Monoid
import           Data.Semigroup
import           Data.Singletons.Prelude
import           Data.Singletons.ShowSing
import           Data.Singletons.Sigma
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
singletons [d| newtype MoodReport = MR (Mood, Intensity) |]


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


computeIntensity :: Intensity -- ^
  -> Intensity -- ^
  -> Intensity
computeIntensity x y | fromEnum x == fromEnum y = x
                     | fromEnum x > fromEnum y = x
                     | otherwise = y


instance Semigroup Intensity where
  x <> y = computeIntensity x y


instance Monoid Intensity where
  mappend = (<>)
  mempty = None


-- addMoodReport :: SMR ()

data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great deriving (Show, Eq, Ord, Enum)



someFunc :: IO ()
someFunc = putStrLn "building ..."
