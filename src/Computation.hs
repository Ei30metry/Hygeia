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

import           Config
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

-- write the Read instance with the help of a parser

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

-- instance (Eq a, Eq a', a' ~ Mood,  a ~ Mood, b ~ Intensity, c ~ Intensity) => Semigroup MoodReport where
--   (MR (a, b)) <> (MR (a, c)) | a == a' = Right (a , c <> b)
--                              | otherwise = Left []



data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great deriving (Show, Eq, Ord, Enum)



someFunc :: IO ()
someFunc = putStrLn "building ..."

-- prettyMatrix :: Show a => Matrix a -> String
-- prettyMatrix m = concat
--    [ "┌ ", unwords (replicate (ncols m) blank), " ┐\n", unlines [ "│ " ++ unwords (fmap (\j -> fill $ strings ! (i,j)) [1..ncols m]) ++ " │" | i <- [1..nrows m] ], "└ ", unwords (replicate (ncols m) blank), " ┘" ]
