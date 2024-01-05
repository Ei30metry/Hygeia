module Parser.Entry( parseEntries, parseDay ) where


import           Computation           ( Alcohol (..), Cigarette (Cigarette),
                                         Entry (..), Intensity (..),
                                         Meditation (..), Mood (..),
                                         Productivity (..), Rating (..),
                                         Sleep (..), Name, Moods )

import           Control.Lens          ( bimap )
import           Control.Monad         ( (<=<), (=<<) )
import           Control.Monad.Except  ( liftEither )

import           Data.ByteString.Char8 ( ByteString, unpack )
import           Data.Functor          ( (<&>) )
import           Data.List             ( sortOn )
import           Data.Time             ( Day, DiffTime, secondsToDiffTime )
import           Data.Vector           ( fromList )

import           Parser.Monad

import           Text.Read             ( readEither )

data SomeEntry where
  SomeEntry :: forall b. b -> SomeEntry


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

-- parses the name section
name :: Parser String
name = string "Name :"

-- parses the name section
parseNameEntry :: Parser Name
parseNameEntry = do
    name
    spaces
    userName <- many1 alphaNum
    spaces
    userLName <- many1 alphaNum
    many newline
    return (userName ++ " " ++  userLName)

-- parses the date section
date :: Parser String
date = string "Date :" <|> string "Date:"


dateSep :: Parser Char
dateSep = char '-'

parseDay :: Parser Day
parseDay = many (digit <|> dateSep) >>= liftEither . readEither

parseDateEntry :: Parser Day
parseDateEntry = do
  date
  spaces
  day <- parseDay
  many newline
  return day

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
parseMoodEntries :: Parser (String ,SomeEntry)
parseMoodEntries = do
  mood
  many newline
  moods <- fromList <$> many1 parseMood <* many newline
  return ("Mood",SomeEntry moods)


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
parseSleepEntry :: Parser (String, SomeEntry)
parseSleepEntry = do
  sleep
  many newline
  (string "Wake up :" <* spaces) <|> (string "wake up :" <* spaces)
  wakeUpTime <- time
  many newline
  (string "Sleep :" <* spaces) <|> (string "sleep :" <* spaces)
  sleepTime <- time
  many newline
  return  ("Sleep", SomeEntry $ SP (secondsToDiffTime wakeUpTime) (secondsToDiffTime sleepTime))


-- alcohol header
alcohol :: Parser String
alcohol = header "Alcohol"


-- NOTE: add parseAlcoholEntries
-- parses the alcohol header and the data in it
parseAlcoholEntry :: Parser (String ,SomeEntry)
parseAlcoholEntry = do
  alcohol
  many newline
  drink <- (many1 alphaNum <* spaces) <* string ":"
  shots <- liftEither . readEither =<< (spaces *> many1 digit)
  many newline
  return ("Alcohol", SomeEntry $ Alcohol drink shots)

-- -- parses the cigarette header
cigarette :: Parser String
cigarette = header "Cigarette"


-- parses the cigarette header and the data in it
parseCigaretteEntry :: Parser (String,SomeEntry)
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
  return ("Cigarette", SomeEntry $ Cigarette number nicotine tar)

-- | Parses the meditation header
meditation :: Parser String
meditation = header "Meditation"

-- parses the meditatin header and the data in it
parseMeditationsEntry :: Parser (String,SomeEntry)
parseMeditationsEntry = do
  meditation
  many newline
  meditations <- many1 (many1 (digit <|> char ':') <* many newline)
  many newline
  return ("Meditation" ,SomeEntry . Med $ fromList meditations)

-- | Parses the productivity header
productivity :: Parser String
productivity = header "Productivity"

-- parses the productivity header and the information in it
parseProductivityEntry :: Parser (String, SomeEntry)
parseProductivityEntry = do
  productivity
  many newline
  done <- liftEither . readEither =<< many1 digit <* char '/'
  shouldHave <- liftEither . readEither =<< many1 digit
  many newline
  return $ ("Productivity", SomeEntry $ Pro (done,shouldHave))

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
parseRatingEntry :: Parser (String,SomeEntry)
parseRatingEntry = rating >> many newline >> parseRating' >>= return . ("Rating", ) . SomeEntry 


-- parses the Entry written by the user (order of the entry doesn't matter)
-- parse headers, figure out the entries that were written, fold the function list on kleisli composition and then apply everythigng to the parsed text
parseEntries :: Parser [(String,SomeEntry)]
parseEntries = undefined -- do
  -- name <- parseNameEntry
  -- day <- parseDateEntry
  -- many1 . choice $ map try [parseAlcoholEntry,parseCigaretteEntry,parseMeditationsEntry,parseMoodEntries,parseRatingEntry,parseProductivityEntry]
  
  
{-
parse headers, accumulate everything between them in the fst of a tuple, then by pattern matching on fst, we get to find what parser we will have to apply, and how we should crewate the Entry type. 

parseHeaders :: Parser [String]
parseHeaders = many1 (skipNonHeaders *> headers) 
  where
    skipNonHeaders = (try $ many (noneOf "[")) 
    headers = choice $ map try [rating,productivity,alcohol,meditation,cigarette,mood,sleep]

-}
