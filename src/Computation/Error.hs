-- |

module Computation.Error where


data EntryError = TooProductive String
                | WrongDate String
                | UnknownMood String
                | MeditationError String
                | CouldntRead String
                | NoRatingH 
                | NoProductivityH 
                | NoMoodH 
                | NoSleepH 
                | NoCigaretteH 
                | NoDrinkH 
                | NoMeditationH
                deriving Eq


instance Show EntryError where
  show (TooProductive x)   = "You can't be this productive! : " <> show x
  show (WrongDate x)       = "Date format is wrong: " <> x
  show (UnknownMood x)     = "Unkonwn Meditation: " <> x
  show (MeditationError x) = "Not a valid meditation input: " <> x
  show NoRatingH           = "Couldn't find Rating header"
  show NoProductivityH     = "Couldn't find Productivity header"
  show NoMoodH             = "Couldn't find Mood header"
  show NoSleepH            = "Couldn't find Sleep header"
  show NoCigaretteH        = "Couldn't find Cigarette header"
  show NoDrinkH            = "Couldn't find Drink header"
  show NoMeditationH       = "Couldn't find Meditaiton header"
