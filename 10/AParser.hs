{-# LANGUAGE TupleSections #-}

module AParser where

import           Control.Applicative

import           Data.Char

import           Data.List.Split

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
        | null ns   = Nothing
        | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

type Name = String
data Employee = Emp { name :: Name, phone :: String }

parseName :: Parser Name
parseName = Parser f
    where 
        f name
            | checkStr name = Just (name, "")
            | otherwise = Nothing
            where a = ['a'..'z'] ++ ['A'..'Z']
                  checkStr str = and $ map (`elem` a) str

parsePhone :: Parser String
parsePhone = Parser f
    where 
        f number
            | checkNum number = Just (number, "")
            | otherwise = Nothing
            where a = ['0'..'9'] ++ ['-', '.', '(', ')']
                  checkNum str = and $ map (`elem` a) str

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f parser = Parser (fmap (first f) . runParser parser)

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p <*> q = Parser $ \s ->
            case (runParser p s) of 
                Nothing -> Nothing
                Just (f, s') -> case (runParser q s') of
                    Nothing -> Nothing
                    Just (x, s'') -> Just (f x, s'')

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = pure (const ()) <*> abParser

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt 

--instance Alternative Maybe where
--    empty = Nothing
--    Nothing <|> p = p
--    Just x <|> _ = Just x

instance Alternative Parser where
    empty = Parser $ const Nothing
    p <|> q = Parser $ \s -> runParser p s <|> runParser q s

--intOrUppercase :: Parser ()
--intOrUppercase = pure (const ()) <*> posInt <|> pure (const ()) <*> (satisfy isUpper)

intOrUppercase = pure (const ()) <*> (satisfy isUpper) <|> pure (const ()) <*> posInt

