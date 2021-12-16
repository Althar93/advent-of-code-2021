module Parser (
        Parser(..),
        Alternative(..),
        runParser,
        parseChar,
        pSelect,
        pIterate,
        pRepeat
    ) where

import Control.Applicative
import Debug.Trace

-- A mini parser
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- Make the Parser an instance of Functor so we may map things
instance Functor Parser where
    -- (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \s -> [(f a, s') | (a, s') <- p s]

-- Make the Parser an instance of Applicative so we may lift it
instance Applicative Parser where
    -- a -> Parser a
    pure = return
    -- Parser (a -> b) -> Parser a -> Parser b
    (<*>) (Parser ab) (Parser b) = Parser $ \s -> [(f a, s'') | (f, s') <- ab s, (a, s'') <- b s']

-- Make the Parser an instance of Monad so that we may use the 'do' notation
instance Monad Parser where
    -- a -> Parser a
    return a = Parser $ \s -> [(a, s)]
    -- Parser a -> (a -> Parser b) -> Parser b
    (>>=) p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') (parse p s)

-- Make the Parser an instance of Alternative so that we may process choice
instance Alternative Parser where
    -- Parser a
    empty = trace "empty" Parser $ \s -> []
    -- Parser a -> Parser a -> Parser a 
    (<|>) p p' = Parser $ \s ->
        case parse p s of 
            []  -> parse p' s
            r   -> r

-- Runs the parser
runParser :: Parser a -> String -> a
runParser p s = case parse p s of 
    [(r, _)] -> r
    _        -> error "Failed to parse string"

-- Parses a single char
parseChar :: Parser Char
parseChar = Parser $ \s -> case s of
    []      -> []
    (c:s')  -> [(c, s')]

-- Selects either using parser a or b depending on the state of the flag
pSelect :: Bool -> Parser a -> Parser b -> Parser (Either a b)
pSelect True pa _   = fmap Left pa
pSelect False _ pb  = fmap Right pb

-- Runs the parser until it can no longer do so
pIterate :: Parser a -> Parser [a]
pIterate p = many p

-- Runs the parser a number of times and returns an array of results
pRepeat :: Int -> Parser a -> Parser [a]
pRepeat 0 _ = pure []
pRepeat n p = do
    a <- p
    fmap (a:) (pRepeat (n - 1) p)
