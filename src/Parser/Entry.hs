module Parser.Entry( parseEntry, parseDay
                   , parseProductivity, parseMeditations
                   , parseRating, parseCigarettes, parseDrinks
                   , parseMoods, parseSleep, time ) where

import           Computation           ( Cigarette (Cigarette), Drink (Drink),
                                         Entry (..), Intensity (..),
                                         Meditation (..), Mood (..), Name,
                                         Productivity (..), Rating (..),
                                         Sleep (..), Stage (..), mkMeditation )
import           Computation.Error

import           Control.Applicative   ( liftA2, liftA3 )
import           Control.Lens          ( bimap )
import           Control.Monad         ( guard, when, (<=<), (=<<), (>=>) )
import           Control.Monad.Except  ( Except, MonadError(..), liftEither, withError )

import           Data.ByteString.Char8 ( ByteString, unpack )
import           Data.Coerce
import           Data.Foldable         ( find )
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
    handleTime hour minute
  where
    handleTime h m | h >= 24 || m >= 59 = throwError . InvalidTime $ show h ++ ":" ++ show m
                   | otherwise = return time'
      where
        time' = h * 3600 + m * 60


-- | computes the time of Sleep and Wake up
header :: String -> Parser String
header h = string $ mconcat ["[", h, "]"]

-- parses the date section
date :: Parser String
date = string "Date :"


dateSep :: Parser Char
dateSep = char '-'
       <|> char '/'
       <|> char '\\'


parseDate :: Parser Day
parseDate = withError (\(CouldntRead x) -> InvalidDate x)
                      (many (digit <|> dateSep) >>= readExcept)

-- | Parses the day header, at the top of the file
parseDay :: Parser Day
parseDay = do
  date
  spaces
  day <- parseDate
  many newline
  return day


parseIntensity :: Parser Intensity
parseIntensity = withError (\(CouldntRead x) -> UnknownIntensity x)
                           (readExcept =<< many letter)

-- | Parses all the mood data constructors, and returns a function
parseMoodName :: Parser (Intensity -> Mood)
parseMoodName = moods >>= \case
      "Neutral" -> return (const Neutral)
      "Angry"   -> return Angry
      "Sad"     -> return Sad
      "Excited" -> return Excited
      "Happy"   -> return Happy
      x         -> throwError (UnknownMood x)
  where
    moods = choice $ map string ["Neutral", "Angry", "Sad"
                                ,"Excited", "Happy"] ++ [many1 alphaNum] -- FIXME ugly hack.


-- | Parses a single mood
parseMood :: Parser Mood
parseMood = do
  userMood <- parseMoodName
  case userMood None of
    Neutral -> many1 newline >> return Neutral
    x       -> do
              spaces
              char ':'
              spaces
              moodIntensity <- parseIntensity
              many1 newline
              return $ userMood moodIntensity


-- | Parses Mood header and its data
parseMoods :: Parser Header
parseMoods = do
  header "Mood"
  many1 newline
  moods <- many1 parseMood
  return (HMoods moods)

-- | Parses the sleep header and its data
parseSleep :: Parser Header
parseSleep = do
    header "Sleep"
    many1 newline
    wakeUpTime <- parseWakeUp <* many1 newline
    sleepTime <- parseSleep <* many1 newline
    return . HSleep $ SP (secondsToDiffTime wakeUpTime)
                         (secondsToDiffTime sleepTime)
  where
   parseWakeUp = choice [string "Wake up :", string "wake up :"] >> spaces >> time
   parseSleep = choice [string "Sleep :", string "sleep :"] >> spaces >> time

-- | Parses the drink header and the data in it
parseDrink :: Parser Drink
parseDrink = do
  drink <- (many1 alphaNum <* spaces) <* string ":"
  shots <- readExcept =<< (spaces *> many1 digit)
  many1 newline
  return $ Drink drink shots

-- | Parses the drink header and its data
parseDrinks :: Parser Header
parseDrinks = do
  header "Drink"
  many1 newline
  drinks <- many1 parseDrink
  return (HDrinks drinks)


parseCigarette :: Parser Cigarette
parseCigarette = do
    cigName <- parseCigName <* many1 newline
    number <- parseCigNumber <* many1 newline
    nicotine <- parseCigNicotine <* many1 newline
    tar <- parseCigTar <* many1 newline
    return (Cigarette cigName number nicotine tar)
  where
    parseCigName = choice [string "Name :", string "name :"] >> spaces
                 >> many1 (alphaNum <|> char ' ')

    parseCigNumber = choice [string "Number :", string "number :"] >> spaces
                   >> many1 digit >>= readExcept

    parseCigNicotine = choice [string "Nicotine :", string "nicotine :"] >> spaces
                     >> many1 stringFloat >>= readExcept

    parseCigTar = choice [string "Tar :", string "tar :"] >> spaces
                >> many1 stringFloat >>= readExcept

-- | Parses the cigarette header and the data in it
parseCigarettes :: Parser Header
parseCigarettes = do
  header "Cigarette"
  many1 newline
  cigarettes <- many parseCigarette
  return (HCigarettes cigarettes)

-- Parses the meditatin header and the data in it
parseMeditations :: Parser Header
parseMeditations = do
  header "Meditation"
  many newline
  meds <- liftEither . traverse mkMeditation =<< many1 (many1 digit <* many1 newline)
  many newline
  return . HMeditations $ meds

-- | Parses the productivity header and the information in it
parseProductivity :: Parser Header
parseProductivity = do
    header "Productivity"
    many newline
    numerator <- many1 digit <* char '/'
    denomerator <- many1 digit
    done <- readExcept numerator
    shouldHaveDone <- readExcept denomerator
    if shouldHaveDone == 0
      then throwError DivisionByZero
      else handleProductivity done shouldHaveDone
  where
    handleProductivity d sd | d >= sd = throwError (TooProductive . show $ productivity)
                            | otherwise = many newline >> (return . HProductivity $ productivity)
        where
          productivity = Pro (d % sd)

-- | Parses the whole Rating header (section)
parseRating :: Parser Header
parseRating = HRating <$> (header "Rating" >> many newline >> many letter >>= readExcept)

-- | Parses the Entry written by the user (order of the entry doesn't matter)
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
