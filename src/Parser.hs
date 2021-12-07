module Parser (
        Parser(..),
        runParser
    ) where

-- A basic generic parser
newtype Parser a = Parser { parse :: String -> (Maybe a, String) }

fromJust :: Maybe a -> a
fromJust Nothing    = error "Undefined"
fromJust (Just a)   = a

instance Functor Parser where
    fmap f p = Parser (\s -> (\(ma, s') -> (Just (f (fromJust ma)), s')) (parse p s))

instance Applicative Parser where
    pure = return
    -- #TODO : My brain hurts... can this be written in a more legible way?
    pf <*> p = Parser (\s -> (\(ma, s') -> (\(mf, s'') -> (Just ((fromJust mf) (fromJust ma)), s'')) (parse pf s')) (parse p s))

unit :: a -> Parser a
unit a = Parser (\s -> (Just a, s))

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser (\s -> (\(ma, s') -> parse (f (fromJust ma)) s') (parse p s))

-- Make an instance of Monad so we can chain parsers together
instance Monad Parser where
    return = unit
    (>>=)  = bind

-- Runs the parser
runParser :: Parser a -> String -> Maybe a
runParser p s = x where 
    (x, _) = parse p s

-- Parses a single item (char)
item :: Parser Char
item = Parser $ \s ->
    case s of
        []      -> (Nothing, [])
        (c:cs)  -> (Just c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c -> if p c then unit c else (Parser (\s -> (Nothing, s)))
