module Parser.Entry( parseEntry, parseDay, parseProductivity, parseMeditations) where

import           Computation           ( Cigarette (Cigarette), Drink (Drink),
                                         Entry (..), Intensity (..),
                                         Meditation (..), Mood (..), Name,
                                         Productivity (..), Rating (..),
                                         Sleep (..), Stage (..), mkMeditation )

import           Control.Applicative   ( liftA2, liftA3 )
import           Control.Lens          ( bimap )
import           Control.Monad         ( (<=<), (=<<) )
import           Control.Monad.Except  ( Except, MonadError, liftEither )

import           Data.ByteString.Char8 ( ByteString, unpack )
import           Data.Coerce
import           Data.Foldable         ( find )
import           Data.Functor          ( (<&>) )
import           Data.List             ( sortOn )
import           Data.Ratio
import           Data.Time             ( Day, DiffTime, secondsToDiffTime )

import           Parser.Monad
import           Parser.Types

import           Text.Read             ( readEither )


stringFloat :: Parser Char
stringFloat = digit <|> char '.'

-- | parses time in format of HH:MM
time :: Parser Integer
time = do
  hour <- readExcept =<< many1 digit
  many1 (char ':')
  minute <- readExcept =<< many1 digit
  return $ (hour * 3600) + (minute * 60)

-- | computes the time of Sleep and Wake up
header :: String -> Parser String
header h = string $ mconcat ["[", h, "]"]

-- parses the date section
date :: Parser String
date = string "Date :" <|> string "Date:"


dateSep :: Parser Char
dateSep = char '-'


parseDate :: Parser Day
parseDate = many (digit <|> dateSep) >>= readExcept


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
  HMoods <$> many1 parseMood <* many newline


-- | Parses Intensities
parseIntensity :: Parser Intensity
parseIntensity = intensities >>= \case
    "Low"     -> return Low
    "Medium"  -> return Medium
    "High"    -> return High
    "Extreme" -> return Extreme
    x         -> throwError ("Unknown Intensity " <> "'" <> show x <> "'")
  where
    intensities = choice $ map string ["Low", "Medium", "High", "Extreme"]

-- | parses the sleep header
sleep :: Parser String
sleep = header "Sleep"


-- | parses the sleep header and its data
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

-- drink header
drink :: Parser String
drink = header "Drink"

-- parses the drink header and the data in it
parseDrink :: Parser Drink
parseDrink = do
  drink <- (many1 alphaNum <* spaces) <* string ":"
  shots <- readExcept =<< (spaces *> many1 digit)
  return $ Drink drink shots

-- | parses the drink header and its data
parseDrinks :: Parser Header
parseDrinks = do
  drink
  many newline
  drinks <- many1 parseDrink
  many newline
  return (HDrinks drinks)

-- parses the cigarette header
cigarette :: Parser String
cigarette = header "Cigarette"

-- parses the cigarette header and the data in it
parseCigarette :: Parser Cigarette
parseCigarette = do
  cigarette
  many newline
  (string "Name :" <* spaces) <|> (string "name :" <* spaces)
  cigName <- many1 (alphaNum <|> char ' ')
  many newline
  (string "Number :" <* spaces) <|> (string "number :" <* spaces)
  number <- readExcept =<< many1 digit
  many newline
  (string "Nicotine :" <* spaces) <|> (string "nicotine :" <* spaces)
  nicotine <- readExcept =<< many1 stringFloat
  many newline
  (string "Tar :" <* spaces) <|> (string "tar :" <* spaces)
  tar <- readExcept =<< many1 stringFloat
  return (Cigarette cigName number nicotine tar)


parseCigarettes :: Parser Header
parseCigarettes = do
  cigarette
  many newline
  cigarettes <- many1 parseCigarette
  many newline
  return . HCigarettes $ cigarettes

-- | Parses the meditation header
meditation :: Parser String
meditation = header "Meditation"

-- parses the meditatin header and the data in it
parseMeditations :: Parser Header
parseMeditations = do
  meditation
  many newline
  meds <- liftEither . traverse mkMeditation =<< many (many1 digit <* many newline)
  many newline
  return . HMeditation $ meds

-- | Parses the productivity header
productivity :: Parser String
productivity = header "Productivity"

-- parses the productivity header and the information in it
parseProductivity :: Parser Header
parseProductivity = do
  productivity
  many newline
  done <- many1 digit <* char '/'
  shouldHaveDone <- many1 digit
  productivity <- liftA2 (%) (readExcept done) (readExcept shouldHaveDone)
  many newline
  return $ HProductivity (Pro productivity)

-- | parses the rating header
rating :: Parser String
rating = header "Rating"


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

-- | parses the whole Rating header (section)
parseRating :: Parser Header
parseRating = (rating >> many newline >> parseRating') <&> HRating

-- | parses the Entry written by the user (order of the entry doesn't matter)
parseEntry :: Parser (Entry Parsed)
parseEntry = do
    day <- parseDay
    headers <- many1 (choice parsers)
    Entry day <$> findE @[Mood] headers
              <*> findE @Sleep headers
              <*> findE @Productivity headers
              <*> findE @[Meditation] headers
              <*> findE @[Drink] headers
              <*> findE @[Cigarette] headers
              <*> findE @Rating headers
 where
   parsers = map try [ parseMoods, parseSleep
                     , parseDrinks, parseMeditations, parseCigarettes
                     , parseProductivity, parseRating]
