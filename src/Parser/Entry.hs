module Parser.Entry( parseEntries, parseDay ) where


import           Computation                   ( Alcohol (..),
                                                 Cigarette (Cigarette),
                                                 Entry (..), Intensity (..),
                                                 Meditation (..), Mood (..),
                                                 Productivity (..), Rating (..),
                                                 Sleep (..) )

import           Control.Monad                 ( (<=<), (=<<) )
import           Control.Monad.Except          ( Except, MonadError (..),
                                                 liftEither )
import           Control.Monad.Trans

import           Data.ByteString.Char8         ( ByteString )
import           Data.Functor                  ( (<&>) )
import           Data.List                     ( sortOn )
import           Data.Time

import           Text.Parsec                   ( ParsecT, try )
import           Text.ParserCombinators.Parsec ( alphaNum, char, choice, digit,
                                                 many, many1, newline, sepBy,
                                                 spaces, string, (<|>) )
import           Text.Read                     ( readEither )



type Parser a = ParsecT ByteString () (Except String) a


stringFloat :: Parser Char
stringFloat = digit <|> char '.'

-- parses time in format of HH:MM
time :: Parser String
time = many1 digit <> many1 (char ':') <> many1 digit

-- computes the time of Sleep and Wake up
header :: String -> Parser String
header h = string $ mconcat ["[", h, "]"]

-- parses the name section
name :: Parser String
name = string "Name :"

-- parses the name section
parseNameEntry :: Parser Entry
parseNameEntry = do
    name
    spaces
    userName <- many1 alphaNum
    spaces
    userLName <- many1 alphaNum
    many newline
    return . EName $ (userName ++ " " ++  userLName)

-- parses the date section
date :: Parser String
date = string "Date :" <|> string "Date:"


dateSep :: Parser Char
dateSep = char '-'

-- FIX partial
parseDay :: Parser Day
parseDay = many (digit <|> dateSep) >>= liftEither . readEither

parseDateEntry :: Parser Entry
parseDateEntry = do
  date
  spaces
  day <- parseDay
  many newline
  return (EDay day)

mood :: Parser String
mood = header "Mood"


-- Parses all the mood data constructors as stirngs
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


-- | Parses Mood
parseMoodEntries :: Parser Entry
parseMoodEntries = do
  mood
  many newline
  EMoodS <$> many1 parseMood <* many newline


-- | Parses Intensities
parseIntensity :: Parser Intensity
parseIntensity = intensities >>= \case
  "Low"     -> return Low
  "Medium"  -> return Medium
  "High"    -> return High
  "Extreme" -> return Extreme
  x         -> throwError ("Unknown Intensity " <> "'" <> show x <> "'")
  where intensities = choice $ map string ["Low", "Medium", "High", "Extreme"]

-- -- parses the sleep header
sleep :: Parser String
sleep = header "Sleep"


-- -- parses the sleep header and it's data
parseSleepEntry :: Parser Entry
parseSleepEntry = do
  sleep
  many newline
  (string "Wake up :" <* spaces) <|> (string "wake up :" <* spaces)
  wakeUpTime <- time
  many newline
  (string "Sleep :" <* spaces) <|> (string "sleep :" <* spaces)
  sleepTime <- time
  many newline
  return . ESleep $ blah (wakeUpTime, sleepTime)
  where blah = undefined


-- -- alcohol header
alcohol :: Parser String
alcohol = header "Alcohol"


-- NOTE: add parseAlcoholEntries
-- parses the alcohol header and the data in it
parseAlcoholEntry :: Parser Entry
parseAlcoholEntry = do
  alcohol
  many newline
  drink <- (many1 alphaNum <* spaces) <* string ":"
  shots <- liftEither . readEither =<< (spaces *> many1 digit)
  many newline
  return . EAlcohol $ Alcohol drink shots

-- -- parses the cigarette header
cigarette :: Parser String
cigarette = header "Cigarette"


-- parses the cigarette header and the data in it
-- Note: add parseCigaretteEntries
parseCigaretteEntry :: Parser Entry
parseCigaretteEntry = do
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
  return . ECigarette $ Cigarette number nicotine tar

-- | Parses the meditation header
meditation :: Parser String
meditation = header "Meditation"

-- parses the meditatin header and the data in it
parseMeditationsEntry :: Parser Entry
parseMeditationsEntry = do
  meditation
  many newline
  meditations <- many1 (many1 (digit <|> char ':') <* many newline)
  many newline
  return . EMeditation $ Med meditations

-- | Parses the productivity header
productivity :: Parser String
productivity = header "Productivity"

-- parses the productivity header and the information in it
parseProductivityEntry :: Parser Entry
parseProductivityEntry = do
  productivity
  many newline
  done <- liftEither . readEither =<< many1 digit <* char '/'
  shouldHave <- liftEither . readEither =<< many1 digit
  many newline
  return . EProductivity $ Pro (done,shouldHave)

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
parseRatingEntry :: Parser Entry
parseRatingEntry = rating >> many newline >> ERating <$> parseRating'


-- -- parses the Entry written by the user (order of the entry doesn't matter)
parseEntries :: Parser [Entry]
parseEntries = many1 $ choice $ map try parsers
 where parsers = [ parseNameEntry, parseDateEntry, parseMoodEntries, parseSleepEntry
                 , parseAlcoholEntry, parseMeditationsEntry, parseCigaretteEntry
                 , parseProductivityEntry, parseRatingEntry ]
