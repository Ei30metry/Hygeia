-- |

module Parser.Monad (Parser, try, alphaNum, char, choice
                    ,digit, many, many1, newline, sepBy
                    ,spaces, string, (<|>), throwError
                    ,runParser, manyTill, anyChar, skipMany
                    ,satisfy, void, lookAhead, optional, noneOf,eof, readExcept, space, letter) where

import           Control.Monad                 ( join, void, (<=<) )
import           Control.Monad.Except          ( Except (..), MonadError (..),
                                                 liftEither, runExcept )

import           Data.ByteString.Char8         ( ByteString )

import           Text.Parsec                   ( ParseError, ParsecT, anyChar,
                                                 eof, lookAhead, manyTill,
                                                 noneOf, optional, runParserT,
                                                 skipMany, try )
import           Text.ParserCombinators.Parsec ( alphaNum, char, choice, digit,
                                                 many, many1, newline, satisfy,
                                                 sepBy, space, spaces, string,
                                                 (<|>), letter )
import           Text.Read                     ( readEither )

type Parser a = ParsecT ByteString () (Except String) a

runParser :: Parser a -> ByteString -> Either String a
runParser parser = join . convert . runExcept . runParserT parser () ""
  where convert (Right (Left x))  = Right . Left $ show x
        convert (Right (Right y)) = Right (Right y)
        convert (Left z)          = Left z


{-# INLINE readExcept #-}
readExcept :: Read a => String -> Parser a
readExcept = liftEither . readEither
