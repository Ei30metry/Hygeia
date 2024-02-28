-- |

module Parser.Types where

import           Computation.Error
import           Computation.Types

import           Control.Monad.Except

import           Data.Foldable        ( find )

data Header = HProductivity Productivity
            | HMeditations [Meditation]
            | HSleep Sleep
            | HRating Rating
            | HCigarettes [Cigarette]
            | HDrinks [Drink]
            | HMoods [Mood]
            deriving (Show, Eq)


{-# INLINE maybeToEither #-}
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither e (Just x) = Right x


class HasHeader a where
  findE :: MonadError EntryError m => [Header] -> m a


instance HasHeader Rating where
  findE headers = liftEither . maybeToEither NoRatingH
                  $ find (\case HRating _ -> True ; _ -> False) headers
                  >>= \(HRating y) -> return y

instance HasHeader Productivity where
  findE headers = liftEither . maybeToEither NoProductivityH
                  $ find (\case HProductivity _ -> True ; _ -> False) headers
                  >>= \(HProductivity y) -> return y

instance HasHeader [Mood] where
  findE headers = liftEither . maybeToEither NoMoodH
                  $ find (\case HMoods _ -> True ; _ -> False) headers
                  >>= \(HMoods y) -> return y

instance HasHeader Sleep where
  findE headers = liftEither . maybeToEither NoSleepH
                  $ find (\case HSleep _ -> True ; _ -> False) headers
                  >>= \(HSleep y) -> return y

instance HasHeader [Cigarette] where
  findE headers = liftEither . maybeToEither NoCigaretteH
                  $ find (\case HCigarettes _ -> True ; _ -> False) headers
                  >>= \(HCigarettes y) -> return y

instance HasHeader [Drink] where
  findE headers = liftEither . maybeToEither NoDrinkH
                  $ find (\case HDrinks _ -> True ; _ -> False) headers
                  >>= \(HDrinks y) -> return y

instance HasHeader [Meditation] where
  findE headers = liftEither . maybeToEither NoMeditationH
                  $ find (\case HMeditations _ -> True ; _ -> False) headers
                  >>= \(HMeditations y) -> return y
