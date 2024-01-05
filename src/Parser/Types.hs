-- | 

module Parser.Types where

import Computation.Types
import Control.Monad.Except
import Data.Foldable (find)

data Header = HProductivity Productivity
            | HMeditation Meditation
            | HSleep Sleep
            | HRating Rating
            | HCigarette Cigarette
            | HAlcohol Alcohol
            | HMoods Moods
            deriving (Show, Eq)


maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither e (Just x) = Right x

class HasHeader a where
  findE :: MonadError String m => [Header] -> m a


instance HasHeader Rating where
  findE headers = liftEither . maybeToEither "Couldn't find Rating header"
                  $ find (\case HRating _ -> True ; _ -> False) headers
                  >>= \(HRating y) -> return y

instance HasHeader Productivity where 
  findE headers = liftEither . maybeToEither "Couldn't find Productivity header"
                  $ find (\case HProductivity _ -> True ; _ -> False) headers
                  >>= \(HProductivity y) -> return y

instance HasHeader Moods where 
  findE headers = liftEither . maybeToEither "Couldn't find Mood header"
                  $ find (\case HMoods _ -> True ; _ -> False) headers
                  >>= \(HMoods y) -> return y

instance HasHeader Sleep where 
  findE headers = liftEither . maybeToEither "Couldn't find Sleep header"
                  $ find (\case HSleep _ -> True ; _ -> False) headers
                  >>= \(HSleep y) -> return y

instance HasHeader Cigarette where 
  findE headers = liftEither . maybeToEither "Couldn't find Cigarette header"
                  $ find (\case HCigarette _ -> True ; _ -> False) headers
                  >>= \(HCigarette y) -> return y

instance HasHeader Alcohol where 
  findE headers = liftEither . maybeToEither "Couldn't find Alcohol header"
                  $ find (\case HAlcohol _ -> True ; _ -> False) headers
                  >>= \(HAlcohol y) -> return y

instance HasHeader Meditation where 
  findE headers = liftEither . maybeToEither "Couldn't find Meditaiton header"
                  $ find (\case HMeditation _ -> True ; _ -> False) headers
                  >>= \(HMeditation y) -> return y
