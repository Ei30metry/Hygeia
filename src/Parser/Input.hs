module Parser.Input(Header (..),
                   parseEntry
                   ) where


import           Computation

import           Data.List                     ( sortOn )
import           Data.Time                     ( Day )

import           Text.Parsec.Char              ( newline )
import           Text.Parsec.String            ( Parser )
import           Text.ParserCombinators.Parsec ( alphaNum, char, choice, digit,
                                                 many, many1, sepBy, spaces,
                                                 string, try, (<|>) )

-- FIX: How is this useful exactly? this isn't how GADTs work ..............
data Header = NameH Name 
            | DateH Day 
            | MoodH [Mood] 
            | SleepH Sleep 
            | ProductivityH Productivity 
            | MeditationH [Meditation] 
            | AlcoholH Alcohol 
            | CigaretteH Cigarette 
            | RatingH Rating
            deriving (Show, Eq)

stringFloat :: Parser Char
stringFloat = digit <|> char '.'

-- instance Functor (Header a) where
--   fmap f ((l :: a -> Header a) a) = f a

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
-- parseName :: Parser (Header a)
-- parseName = do
--     name
--     spaces
--     userName <- many1 alphaNum
--     spaces
--     userLName <- many1 alphaNum
--     many newline
--     return . NameH $ blah (userName ++ " " ++  userLName)
--   where blah = undefined

-- -- parses the date section
-- date :: Parser String
-- date = string "Date :" <|> string "Date:"


-- dateSep :: Parser Char
-- dateSep = char '-' <|> char '/' <|> char '_' <|> char '\\'


-- -- parses the date section
-- -- parseDate :: forall a st. (a ~ String ) => Parser (Header a)
-- -- parseDate = do
-- --   date
-- --   spaces
-- --   userDate <- many1 (alphaNum <|> dateSep)
-- --   many newline
-- --   return $ Date userDate

-- parseDate :: Parser (Header Day)
-- parseDate = do
--   date
--   spaces
--   year <- many1 alphaNum
--   dateSep
--   month <- many1 alphaNum
--   dateSep
--   day <- many1 alphaNum
--   many newline
--   return . DateH $ blah (year,month,day)
--   where blah = undefined

-- -- parses the mood
-- -- refactor with a list function

-- mood :: Parser String
-- mood = header "Mood"


-- -- Parses all the mood data constructors as stirngs
-- parseMood :: Parser String
-- parseMood = choice $ map string ["Neutral", "Angry", "Sad"
--                                  ,"Excited", "Happy", "Focused", "Bored"]


-- -- parses one mood
-- parseMoodReport :: Parser (String, String)
-- parseMoodReport = do
--   userMood <- parseMood
--   spaces
--   char ':'
--   spaces
--   moodIntensity <- parseIntensity
--   many newline
--   return (userMood, moodIntensity)


-- parseMoodReports :: Parser (Header a)
-- parseMoodReports = do
--   mood
--   many newline
--   l <- many1 parseMoodReport <* many newline
--   return . MoodReportH . blah $ sortOn fst l
--   where blah = undefined


-- -- parses all the possible Intensities
-- parseIntensity :: Parser String
-- parseIntensity = choice $ map string ["Low", "Medium", "High", "Extreme"]

-- -- parses the sleep header
-- sleep :: Parser String
-- sleep = header "Sleep"


-- -- parses the sleep header and it's data
-- parseSleep :: Parser (Header a)
-- parseSleep = do
--   sleep
--   many newline
--   (string "Wake up :" <* spaces) <|> (string "wake up :" <* spaces)
--   wakeUpTime <- time
--   many newline
--   (string "Sleep :" <* spaces) <|> (string "sleep :" <* spaces)
--   sleepTime <- time
--   many newline
--   return . SleepH $ blah (wakeUpTime, sleepTime)
--   where blah = undefined


-- -- alcohol header
-- alcohol :: Parser String
-- alcohol = header "Alcohol"


-- -- parses the alcohol header and the data in it
-- parseAlcohol :: Parser (Header a)
-- parseAlcohol = do
--   alcohol
--   many newline
--   drink <- (many1 alphaNum <* spaces) <* string ":"
--   shots <- spaces *> many1 digit
--   many newline
--   return . AlcoholH $ blah (drink,shots)
--   where blah = undefined

-- -- parses the cigarette header
-- cigarette :: Parser String
-- cigarette = header "Cigarette"


-- -- parses the cigarette header and the data in it
-- parseCigarette :: Parser (Header a)
-- parseCigarette = do
--   cigarette
--   many newline
--   (string "Number :" <* spaces) <|> (string "number :" <* spaces)
--   number <- many1 digit
--   many newline
--   (string "Nicotine :" <* spaces) <|> (string "nicotine :" <* spaces)
--   nicotine <- many1 stringFloat
--   many newline
--   (string "Tar :" <* spaces) <|> (string "tar :" <* spaces)
--   tar <- many1 stringFloat
--   many newline
--   return . CigaretteH $ blah (number,nicotine,tar)
--   where blah = undefined

-- -- | Parses the meditation header
-- meditation :: Parser String
-- meditation = header "Meditation"

-- -- parses the meditatin header and the data in it
-- parseMeditations :: Parser (Header a)
-- parseMeditations = do
--   meditation
--   many newline
--   meditations <- many1 (many1 (digit <|> char ':') <* many newline)
--   many newline
--   return . MeditationH $ blah meditations
--   where blah = undefined

-- -- | Parses the productivity header
-- productivity :: Parser String
-- productivity = header "Productivity"

-- -- parses the productivity header and the information in it
-- parseProductivity :: Parser (Header a)
-- parseProductivity = do
--   productivity
--   many newline
--   done <- many1 digit <* char '/'
--   shouldHave <- many1 digit
--   many newline
--   return . ProductivityH $ blah (done,shouldHave)
--   where blah = undefined

-- -- parses the rating header
-- rating :: Parser String
-- rating = header "Rating"


-- -- parses the different rating a user might give
-- parseRating' :: Parser String
-- parseRating' = choice $ map string ["Great", "Good", "Normal", "Bad", "Awful"]


-- -- parses the whole Rating header (section)
-- parseRating :: Parser (Header String)
-- parseRating = rating >> many newline >> RatingH <$> (blah parseRating')
--   where blah = undefined


-- -- parses the Entry written by the user (order of the entry doesn't matter)
-- parseEntry :: Parser [Header a]
-- parseEntry = do
--   entryParser <- many1 $ choice $ map try listOfParsers
--   return . blah $ entryParser
--  where listOfParsers = [ parseName, parseDate, parseMoodReports, parseSleep
--                        , parseAlcohol, parseMeditations, parseCigarette
--                        , parseProductivity, parseRating ]
--        blah = undefined

parseEntry = undefined
