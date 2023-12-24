module Parser.Input(Header (..),
                   parseEntry
                   ) where


import           Data.List                     ( sortOn )

import           Text.Parsec.Char              ( newline )
import           Text.Parsec.String            ( Parser )
import           Text.ParserCombinators.Parsec ( alphaNum, char,
                                                 choice, digit, many, many1,
                                                 sepBy, spaces, string, try,
                                                 (<|>) )

-- FIX: How is this useful exactly? this isn't how GADTs work ..............
data Header a where
  NameH :: a -> Header a
  DateH :: (a,a,a) -> Header a
  MoodReportH :: [(a,a)] -> Header a -- when writting the show instance, the strings should me mconcated with a newline charecter
  SleepH :: (a,a) -> Header a -- when writting the show instance, the strings should me mconcated with a newline charecter
  ProductivityH :: (a,a) -> Header a
  MeditationH :: [a] -> Header a
  AlcoholH :: (a,a) -> Header a
  CigaretteH :: (a,a,a) -> Header a
  RatingH :: a -> Header a
  AllHeaders :: [Header a] -> Header a


stringFloat :: Parser Char 
stringFloat = digit <|> char '.'

-- instance Functor (Header a) where
--   fmap f ((l :: a -> Header a) a) = f a

instance (Show a) => Show (Header a) where
  show (NameH a)         = show a
  show (DateH a)         = show a
  show (MoodReportH a)   = show a
  show (SleepH a)        = show a
  show (ProductivityH a) = show a
  show (MeditationH a)   = show a
  show (AlcoholH a)      = show a
  show (CigaretteH a)    = show a
  show (RatingH a)       = show a
  show (AllHeaders a)    = show a

-- parses '\n' charecters
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
parseName :: forall a st. (a ~ String) => Parser (Header a)
parseName = do
  name
  spaces
  userName <- many1 alphaNum
  spaces
  userLName <- many1 alphaNum
  many newline
  return $ NameH (userName ++ " " ++  userLName)


-- parses the date section
date :: Parser String
date = string "Date :" <|> string "Date:"


dateSep :: Parser Char
dateSep = char '-' <|> char '/' <|> char '_' <|> char '\\'


-- parses the date section
-- parseDate :: forall a st. (a ~ String ) => Parser (Header a)
-- parseDate = do
--   date
--   spaces
--   userDate <- many1 (alphaNum <|> dateSep)
--   many newline
--   return $ Date userDate

parseDate :: forall a st. (a ~ String) => Parser (Header a)
parseDate = do
  date
  spaces
  year <- many1 alphaNum
  dateSep
  month <- many1 alphaNum
  dateSep
  day <- many1 alphaNum
  many newline
  return $ DateH (year,month,day)

-- parses the mood
-- refactor with a list function

mood :: Parser String
mood = header "Mood"


-- Parses all the mood data constructors as stirngs
parseMood :: Parser String
parseMood = choice $ map string ["Neutral", "Angry", "Sad"
                                 ,"Excited", "Happy", "Focused", "Bored"]


-- parses one mood
parseMoodReport :: Parser (String, String)
parseMoodReport = do
  userMood <- parseMood
  spaces
  char ':'
  spaces
  moodIntensity <- parseIntensity
  many newline
  return (userMood, moodIntensity)



parseMoodReports :: forall a st. (a ~ String) => Parser (Header a)
parseMoodReports = do
  mood
  many newline
  l <- many1 parseMoodReport <* many newline
  return $ MoodReportH $ sortOn fst l



-- parses all the possible Intensities
parseIntensity :: Parser String
parseIntensity = choice $ map string ["Low", "Medium", "High", "Extreme"]

-- parses the sleep header
sleep :: Parser String
sleep = header "Sleep"


-- parses the sleep header and it's data
parseSleep :: forall a st. (a ~ String) => Parser (Header a)
parseSleep = do
  sleep
  many newline
  (string "Wake up :" <* spaces) <|> (string "wake up :" <* spaces)
  wakeUpTime <- time
  many newline
  (string "Sleep :" <* spaces) <|> (string "sleep :" <* spaces)
  sleepTime <- time
  many newline
  return $ SleepH (wakeUpTime, sleepTime)


-- alcohol header
alcohol :: Parser String
alcohol = header "Alcohol"


-- parses the alcohol header and the data in it
parseAlcohol :: forall a st. (a ~ String) => Parser (Header a)
parseAlcohol = do
  alcohol
  many newline
  drink <- (many1 alphaNum <* spaces) <* string ":"
  shots <- spaces *> many1 digit
  many newline
  return $ AlcoholH (drink,shots)


-- parses the cigarette header
cigarette :: Parser String
cigarette = header "Cigarette"


-- parses the cigarette header and the data in it
parseCigarette :: forall a st. (a ~ String) => Parser (Header a)
parseCigarette = do
  cigarette
  many newline
  (string "Number :" <* spaces) <|> (string "number :" <* spaces)
  number <- many1 digit
  many newline
  (string "Nicotine :" <* spaces) <|> (string "nicotine :" <* spaces)
  nicotine <- many1 stringFloat
  many newline
  (string "Tar :" <* spaces) <|> (string "tar :" <* spaces)
  tar <- many1 stringFloat
  many newline
  return $ CigaretteH (number,nicotine,tar)


-- | Parses the meditation header
meditation :: Parser String
meditation = header "Meditation"

-- parses the meditatin header and the data in it
parseMeditations :: forall a st. (a ~ String) => Parser (Header a)
parseMeditations = do
  meditation
  many newline
  meditations <- many1 (many1 (digit <|> char ':') <* many newline)
  many newline
  return $ MeditationH meditations

-- | Parses the productivity header
productivity :: Parser String
productivity = header "Productivity"

-- parses the productivity header and the information in it
parseProductivity :: forall a st. (a ~ String) => Parser (Header a)
parseProductivity = do
  productivity
  many newline
  done <- many1 digit <* char '/'
  shouldHave <- many1 digit
  many newline
  return $ ProductivityH (done,shouldHave)


-- parses the rating header
rating :: Parser String
rating = header "Rating"


-- parses the different rating a user might give
parseRating' :: Parser String
parseRating' = choice $ map string ["Great", "Good", "Normal", "Bad", "Awful"]


-- parses the whole Rating header (section)
parseRating :: Parser (Header String)
parseRating = rating >> many newline >> RatingH <$> parseRating'


-- parses the Entry written by the user (order of the entry doesn't matter)
parseEntry :: Parser (Header String)
parseEntry = do
  entryParser <- many1 $ choice $ map try listOfParsers
  return . AllHeaders $ entryParser
 where listOfParsers = [ parseName, parseDate, parseMoodReports, parseSleep
                       , parseAlcohol, parseMeditations, parseCigarette
                       , parseProductivity, parseRating ]
