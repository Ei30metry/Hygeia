-- | Parser monad used to parse entry files

module Parser.Monad (Parser, try, alphaNum, char, choice
                    ,digit, many, many1, newline, sepBy
                    ,spaces, string, (<|>), throwError
                    ,runParser, manyTill, anyChar, skipMany
                    ,satisfy, void, lookAhead, optional, noneOf,eof, readExcept, space, letter, EntryError(..)
                    ,withError') where

import           Computation.Error
import           Computation.Types

import           Control.Monad                 ( join, void, (<=<) )
import           Control.Monad.Error.Class
import           Control.Monad.Except          ( Except (..), MonadError (..),
                                                 liftEither, runExcept,
                                                 withExcept, withExceptT )
import           Control.Monad.Trans.Class

import           Data.ByteString.Char8         ( ByteString )

import           Text.Parsec                   ( ParseError, ParsecT, anyChar,
                                                 eof, lookAhead, manyTill,
                                                 noneOf, optional, runParserT,
                                                 skipMany, try )
import           Text.ParserCombinators.Parsec ( alphaNum, char, choice, digit,
                                                 letter, many, many1, newline,
                                                 satisfy, sepBy, space, spaces,
                                                 string, (<|>) )
import           Text.Read                     ( readEither )


type Parser a = ParsecT ByteString () (Except EntryError) a


runParser :: Parser a -> ByteString -> Either String a
runParser parser = join . convert . runExcept . runParserT parser () ""
  where convert (Right (Left x))  = Right . Left $ show x
        convert (Right (Right y)) = Right (Right y)
        convert (Left z)          = Left $ show z

-- Copied from mtl version 2.3.1
withError' :: MonadError e m => (e -> e) -> m a -> m a
withError' f action = tryError' action >>= either (throwError . f) pure

-- Copied from mtl version 2.3.1
tryError' :: MonadError e m => m a -> m (Either e a)
tryError' action = (Right <$> action) `catchError` (pure . Left)


{-# INLINE readExcept #-}
readExcept :: Read a => String -> Parser a
readExcept x = lift . withExcept (const $ CouldntRead x) . liftEither $ readEither x
