{-# LANGUAGE FlexibleContexts #-}
module Parse where

import qualified Text.Parsec as Parsec
import Data.Functor.Identity

-- alias Parsec.parse for more concise usage in my examples:
parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"

parseLines1 :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m [a]
parseLines1 rule = Parsec.sepEndBy1 rule $ Parsec.char '\n'

parseInt :: Parsec.Parsec String () Int
parseInt = do
    neg <- Parsec.option "" (Parsec.string "-") 
    digs <- Parsec.many1 Parsec.digit
    return $ read $ neg ++ digs