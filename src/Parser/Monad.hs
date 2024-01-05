-- |

module Parser.Monad (Parser, try, alphaNum, char, choice
                    ,digit, many, many1, newline, sepBy
                    ,spaces, string, (<|>), throwError 
                    ,runParser, manyTill, anyChar, skipMany
                    ,satisfy, void, lookAhead, optional, noneOf,eof) where

import           Control.Monad                 ( join, (<=<), void )
import           Control.Monad.Except          ( Except (..), MonadError (..),
                                                 runExcept )

import           Data.ByteString.Char8         ( ByteString )

import           Text.Parsec                   ( ParseError, ParsecT, anyChar,
                                                 manyTill, runParserT, skipMany,
                                                 try, lookAhead, optional, noneOf, eof )
import           Text.ParserCombinators.Parsec ( alphaNum, char, choice, digit,
                                                 many, many1, newline, satisfy,
                                                 sepBy, spaces, string, (<|>) )

type Parser a = ParsecT ByteString () (Except String) a

runParser :: Parser a -> ByteString -> Either String a
runParser parser = join . convert . runExcept . runParserT parser () ""
  where convert (Right (Left x))  = Right . Left $ show x
        convert (Right (Right y)) = Right (Right y)
        convert (Left z)          = Left z
