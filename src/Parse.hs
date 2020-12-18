{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Data.Functor.Identity
import Data.Void (Void)
import Data.Text (Text)
import qualified Text.Parsec as P 
import Text.Parsec (option, char,  digit, string, many1, sepEndBy1, try )
import Control.Applicative (Applicative(liftA2), (<|>))

-- alias Parsec.parse for more concise usage in my examples:
-- parse :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
-- parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse rule = P.parse rule "(source)"

-- parseLines1 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
-- parseLines1 :: (MonadParsec e s m, Token s ~ Char) => m a -> m [a]
parseLines1 rule = sepEndBy1 rule P.newline

parseInt :: P.Parsec String () Int
parseInt = read <$> liftA2 (:) (option ' ' (char '-')) (many1 digit)