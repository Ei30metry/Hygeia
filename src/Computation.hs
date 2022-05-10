{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module Computation where

import           Data.Coerce
import           Data.Kind      (Constraint, Type)
import           Data.List
import           Data.Monoid
import           Data.Semigroup
import           GHC.TypeLits
import           Parser


-- a Type representing one's mood
data Mood = Angry
          | Sad
          | Neutral
          | Happy
          | Excited deriving (Read, Eq, Ord, Show)


data Intensity = None
               | Low
               | Medium
               | High
               | Extreme deriving (Show, Read, Eq, Ord, Enum)

-- I'm using a newtype wrapper to be able to coerce it with the normal tuple when computing the data that comes from files
newtype MoodReport = MR (Mood, Intensity)


instance (a ~ Mood, b ~ Intensity) => Show MoodReport where
  show (MR (a, b)) = show a ++ " : " ++ show b


computeIntensity :: Intensity -> Intensity -> Intensity
computeIntensity x y | fromEnum x == fromEnum y = x
                     | fromEnum x > fromEnum y = x
                     | otherwise = y


instance Semigroup Intensity where
  x <> y = computeIntensity x y


instance Monoid Intensity where
  mappend = (<>)
  mempty = None

instance (a ~ Mood, b ~ Intensity, c ~ Intensity) => Semigroup MoodReport where
  (MR (a, b)) <> (MR (a, c)) = (a , c <> b)



data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great deriving (Show, Eq, Ord, Enum)


-- data Header (a :: Symbol) =
--               Name a
--             | Date a
--             | MoodH [a]
--             | Sleep [a]
--             | Productivity [a]
--             | Meditation [a]
--             | Rating a deriving (Eq, Enum)


type Minutes = Int

-- heterogenous list

-- data HList (a :: Header t) where
--   HNil :: HList t
--   (:>) :: t -> HList ts -> HList (t ': ts)

-- infixr 5 :>



-- type family RecursiveInstance (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
--   RecursiveInstance c '[] = ()
--   RecursiveInstance c (t ': ts) = (c t, RecursiveInstance c ts)


-- instance RecursiveInstance Show ts => Show (HList ts) where
--   show HNil      = "HNil"
--   show (a :> as) = show a ++ show as


someFunc :: IO ()
someFunc = putStrLn "building ..."

-- prettyMatrix :: Show a => Matrix a -> String
-- prettyMatrix m = concat
--    [ "┌ ", unwords (replicate (ncols m) blank), " ┐\n", unlines [ "│ " ++ unwords (fmap (\j -> fill $ strings ! (i,j)) [1..ncols m]) ++ " │" | i <- [1..nrows m] ], "└ ", unwords (replicate (ncols m) blank), " ┘" ]
