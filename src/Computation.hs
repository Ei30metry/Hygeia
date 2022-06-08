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
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RoleAnnotations          #-}
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
import           Data.Coerce                       (Coercible)
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
import           Data.Type.Equality                (TestEquality (testEquality))
import           Data.Typeable
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

-- using newtype to be able to coerce
singletons [d| newtype MoodReport = MR (Mood, Intensity) deriving Show |]



singletons [d| computeIntensity :: Intensity -> Intensity -> Intensity
               computeIntensity x y | fromEnum x == fromEnum y = x
                                    | fromEnum x > fromEnum y = x
                                    | otherwise = y |]


singletons [d| instance Semigroup Intensity where
                 x <> y = computeIntensity x y |]

-- singletons [d| instance Semigroup Intensity where
--                  x <> y = computeIntensity x y |]



-- monoid instance for Intensity
singletons [d| instance Monoid Intensity where
                 mappend = (<>)
                 mempty = None |]


-- Rating data type for rating the day
data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great deriving (Show, Eq, Ord, Enum)



-- this function will only work if our SMoods are the same
addSingMoodReports :: forall a a' (b :: Intensity) (c :: Intensity). SMoodReport ('MR '(a, b)) -> SMoodReport ('MR '(a, c)) -> SMoodReport ('MR '(a, b <> c))
addSingMoodReports (SMR (STuple2 a b)) (SMR (STuple2 a' c)) = SMR $ STuple2 a (b %<> c)



-- safe function to add MoodReports
addMoodReports :: MoodReport -> MoodReport -> Maybe MoodReport
addMoodReports (FromSing l@(SMR (STuple2 a b))) (FromSing r@(SMR (STuple2 a' b'))) = do
  Refl <- testEquality a a'
  return $ FromSing $ addSingMoodReports l r



someFunc :: IO ()
someFunc = putStrLn "building ..."
