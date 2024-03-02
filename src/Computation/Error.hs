-- |

module Computation.Error where


data EntryError = TooProductive String
                | InvalidDate String
                | UnknownMood String
                | InvalidTime String
                | UnknownIntensity String
                | MeditationError String
                | CouldntRead String
                | NoIntensityForNeutral
                | DivisionByZero
                | NoRatingH 
                | NoProductivityH 
                | NoMoodH 
                | NoSleepH 
                | NoCigaretteH 
                | NoDrinkH 
                | NoMeditationH
                deriving Eq


instance Show EntryError where
  show (TooProductive x)     = "You can't be this productive! : " <> x
  show (UnknownMood x)       = "Unknown Mood: " <> x
  show (UnknownIntensity x)  = "Unknown Intensity: " <> x
  show NoIntensityForNeutral = "You can't write an intensity for Neutral!"
  show (InvalidTime x)       = "Not a valid time: " <> x
  show (MeditationError x)   = "Not a valid meditation input: " <> x
  show (InvalidDate x)       = "Not a valid date: " <> x
  show (CouldntRead x)       = "Couldn't read " <> x
  show NoRatingH             = "Couldn't find Rating header"
  show NoProductivityH       = "Couldn't find Productivity header"
  show DivisionByZero        = "Can't divide by zero!"
  show NoMoodH               = "Couldn't find Mood header"
  show NoSleepH              = "Couldn't find Sleep header"
  show NoCigaretteH          = "Couldn't find Cigarette header"
  show NoDrinkH              = "Couldn't find Drink header"
  show NoMeditationH         = "Couldn't find Meditaiton header"
