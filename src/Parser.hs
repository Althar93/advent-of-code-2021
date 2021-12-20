module Parser (
        Parser(..),
        Alternative(..),
        runParser,
        parseItem,
        parseChar,
        parseDigit,
        parseString,
        parseInt,
        parseSpaces,
        pEither,
        pSelect,
        pIterate,
        pRepeat
    ) where

import Control.Applicative
import Data.Char

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
    empty = Parser $ \s -> []
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

-- Parses a single item
parseItem :: Parser Char
parseItem = Parser $ \s -> case s of
    []      -> []
    (c:s')  -> [(c, s')]

-- Parses the single char
parseChar :: Char -> Parser Char
parseChar c = Parser $ \s -> 
    case s of
        []          -> []
        (c':s)      -> if c == c' 
                        then [(c, s)] 
                        else []

-- Parses a single digit
parseDigit :: Parser Char
parseDigit = Parser $ \s -> 
    case s of
        []          -> []
        (c:s)       -> if isDigit c 
                        then [(c, s)]
                        else []

-- Parses the given string
parseString :: String -> Parser String
parseString []      = pure []
parseString (c:s)   = do
    parseChar c
    parseString s
    return (c:s)

-- Parses an int
parseInt :: Parser Int
parseInt = do
    s   <- parseString "-" <|> return []
    ds  <- some parseDigit
    return $ read (s ++ ds)

-- Parses any space(s)
parseSpaces :: Parser String
parseSpaces = many (parseChar ' ')

-- Runs the parser and tries to populate an either type, first populating the left and then the right if the first fails
pEither :: Parser a -> Parser b -> Parser (Either a b)
pEither pa pb = fmap Left pa <|> fmap Right pb

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
