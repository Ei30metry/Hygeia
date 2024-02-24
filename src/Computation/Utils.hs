-- | Simple utility functions

module Computation.Utils where

import           Config                ( DaemonConf (..) )

import           Control.Applicative
import           Control.Lens

import           Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import           Data.Kind             ( Constraint, Type )
import           Data.List             ( groupBy, sort )
import           Data.Maybe
import           Data.Time             ( Day, DiffTime, diffTimeToPicoseconds )


label f x = (x, f x)

diffTimeToSeconds = (* 10^12) . diffTimeToPicoseconds

{-
NOTE
Because of the fact that some of our types, namely Mood, Cigarette, and Drink,
can't have a total Monoid instance, the neutral typeclass will
be here to help write things that make use of "mempty". In this case, neutral.

Examples of where this is useful: foldr, writing Monoid instances ....

The same reasoning applies to Combinable
-}


class Neutral a where
  neutral :: a


class (Ord a, Neutral a) => Combinable a where
  partialCombine :: a -> a -> a
  combPrecon :: (a -> a -> Bool)

  condense :: [a] -> [a]
  condense = map (foldr partialCombine neutral) . groupBy combPrecon . sort

  (<!>) :: a -> a -> a
  (<!>) = partialCombine

  combine :: a -> a -> Maybe a
  combine x y | x == y = Just $ partialCombine x y
              | otherwise = Nothing
  {-# MINIMAL partialCombine, combPrecon #-}


type family SummaryType a


class Summarizable a where
  summary :: a -> SummaryType a
