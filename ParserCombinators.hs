module ParserCombinators where
import Control.Monad

--PARSERS
data Parser s a = Parser {runP :: s -> [(a,s)]}

instance Monad (Parser s) where
  return a = Parser $ \s -> [(a,s)]
  p >>= q  = Parser $ \s -> concat [runP (q a) s' | (a,s') <- runP p s]

instance Applicative (Parser s) where
    (<*>) = ap
    pure = return

instance Functor (Parser s) where
    fmap = liftM

(<|>) :: Parser s a -> Parser s a -> Parser s a
p <|> q = Parser $ \s -> runP p s ++ runP q s
infixr 6 <|>

pList :: Parser s a -> Parser s [a]
pList p = (:) <$> p <*> pList p
       <|> pSucceed []
pSucceed :: a -> Parser s a
pSucceed a = Parser $ \s -> [(a,s)]
