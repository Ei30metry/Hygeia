-- | Simple utility functions 

module Computation.Utils where

-- | Safe head
head' []     = Nothing
head' (x:xs) = Just x
