module Parser.Entry( parseEntry, parseDay ) where

import           Computation           ( Alcohol (..), Cigarette (Cigarette),
                                         Entry (..), Intensity (..),
                                         Meditation (..), Mood (..), Moods,
                                         Name, Productivity (..), Rating (..),
                                         Sleep (..) )

import           Control.Lens          ( bimap )
import           Control.Monad         ( (<=<), (=<<) )
import           Control.Monad.Except  ( Except, MonadError, liftEither )

import           Data.ByteString.Char8 ( ByteString, unpack )
import           Data.Foldable         ( find )
import           Data.Functor          ( (<&>) )
import           Data.List             ( sortOn )
import           Data.Time             ( Day, DiffTime, secondsToDiffTime )
import           Data.Vector           ( fromList )

import           Parser.Monad
import           Parser.Types

import           Text.Read             ( readEither )


stringFloat :: Parser Char
stringFloat = digit <|> char '.'

-- parses time in format of HH:MM
time :: Parser Integer
time = do
  hour <- liftEither . readEither =<< many1 digit
  many1 (char ':')
  minute <- liftEither . readEither =<< many1 digit
  return $ (hour * 3600) + (minute * 60)

-- computes the time of Sleep and Wake up
header :: String -> Parser String
header h = string $ mconcat ["[", h, "]"]

-- parses the date section
date :: Parser String
date = string "Date :" <|> string "Date:"


dateSep :: Parser Char
dateSep = char '-'


parseDate :: Parser Day
parseDate = many (digit <|> dateSep) >>= liftEither . readEither


parseDay :: Parser Day
parseDay = do
  date
  spaces
  day <- parseDate
  many newline
  return day


mood :: Parser String
mood = header "Mood"

-- | Parses all the mood data constructors as stirngs
parseMood' :: Parser (Intensity -> Mood)
parseMood' = moods >>= \case
      "Neutral" -> return (const Neutral)
      "Angry"   -> return Angry
      "Sad"     -> return Sad
      "Excited" -> return Excited
      "Happy"   -> return Happy
      x         -> throwError ("Unknown Mood " <> "'" <> show x <> "'")
  where
    moods = choice $ map string ["Neutral", "Angry", "Sad"
                                ,"Excited", "Happy"]


-- | Parses a single mood
parseMood :: Parser Mood
parseMood = do
  userMood <- parseMood'
  spaces
  char ':'
  spaces
  moodIntensity <- parseIntensity
  many newline
  return $ userMood moodIntensity


-- | Parses Mood header and its data
parseMoods :: Parser Header
parseMoods = do
  mood
  many newline
  moods <- fromList <$> many1 parseMood <* many newline
  return (HMoods moods)


-- | Parses Intensities
parseIntensity :: Parser Intensity
parseIntensity = intensities >>= \case
  "Low"     -> return Low
  "Medium"  -> return Medium
  "High"    -> return High
  "Extreme" -> return Extreme
  x         -> throwError ("Unknown Intensity " <> "'" <> show x <> "'")
  where intensities = choice $ map string ["Low", "Medium", "High", "Extreme"]

-- | parses the sleep header
sleep :: Parser String
sleep = header "Sleep"


-- | parses the sleep header and it's data
parseSleep :: Parser Header
parseSleep = do
  sleep
  many newline
  (string "Wake up :" <* spaces) <|> (string "wake up :" <* spaces)
  wakeUpTime <- time
  many newline
  (string "Sleep :" <* spaces) <|> (string "sleep :" <* spaces)
  sleepTime <- time
  many newline
  return . HSleep $ SP (secondsToDiffTime wakeUpTime) (secondsToDiffTime sleepTime)

-- alcohol header
alcohol :: Parser String
alcohol = header "Alcohol"

-- TODO: add parseAlcoholEntries
-- parses the alcohol header and the data in it
parseAlcohol :: Parser Header
parseAlcohol = do
  alcohol
  many newline
  drink <- (many1 alphaNum <* spaces) <* string ":"
  shots <- liftEither . readEither =<< (spaces *> many1 digit)
  many newline
  return . HAlcohol $ Alcohol drink shots

-- -- parses the cigarette header
cigarette :: Parser String
cigarette = header "Cigarette"


-- parses the cigarette header and the data in it
parseCigarette :: Parser Header
parseCigarette = do
  cigarette
  many newline
  (string "Number :" <* spaces) <|> (string "number :" <* spaces)
  number <- liftEither . readEither =<< many1 digit
  many newline
  (string "Nicotine :" <* spaces) <|> (string "nicotine :" <* spaces)
  nicotine <- liftEither . readEither =<< many1 stringFloat
  many newline
  (string "Tar :" <* spaces) <|> (string "tar :" <* spaces)
  tar <- liftEither . readEither =<< many1 stringFloat
  many newline
  return . HCigarette $ Cigarette number nicotine tar

-- | Parses the meditation header
meditation :: Parser String
meditation = header "Meditation"

-- parses the meditatin header and the data in it
parseMeditations :: Parser Header
parseMeditations = do
  meditation
  many newline
  meditations <- many1 (many1 (digit <|> char ':') <* many newline)
  many newline
  return . HMeditation . Med $ fromList meditations

-- | Parses the productivity header
productivity :: Parser String
productivity = header "Productivity"

-- parses the productivity header and the information in it
parseProductivity :: Parser Header
parseProductivity = do
  productivity
  many newline
  done <- liftEither . readEither =<< many1 digit <* char '/'
  shouldHave <- liftEither . readEither =<< many1 digit
  many newline
  return . HProductivity $ Pro (done,shouldHave)

-- parses the rating header
rating :: Parser String
rating = header "Rating"

-- parses the different rating a user might give
parseRating' :: Parser Rating
parseRating' = rates >>= \case
    "Great"  -> return Great
    "Good"   -> return Good
    "Normal" -> return Normal
    "Bad"    -> return Bad
    "Awful"  -> return Awful
    x        -> throwError ("Unknown Rating " <> "'" <> show x <> "'")
  where
    rates = choice $ map string ["Great", "Good", "Normal", "Bad", "Awful"]

-- parses the whole Rating header (section)
parseRating :: Parser Header
parseRating = (rating >> many newline >> parseRating') <&> HRating

-- parses the Entry written by the user (order of the entry doesn't matter)
parseEntry :: Parser Entry
parseEntry = do
    day <- parseDay
    headers <- many1 (choice parsers)
    Entry day <$> findE @Moods headers
              <*> findE @Sleep headers
              <*> findE @Productivity headers
              <*> findE @Meditation headers
              <*> findE @Alcohol headers
              <*> findE @Cigarette headers
              <*> findE @Rating headers
 where
   parsers = map try [ parseMoods, parseSleep
                     , parseAlcohol , parseMeditations, parseCigarette
                     , parseProductivity, parseRating]
