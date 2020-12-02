{-# LANGUAGE FlexibleContexts #-}
module Parse where

import qualified Text.Parsec as Parsec

-- alias Parsec.parse for more concise usage in my examples:
parse rule = Parsec.parse rule "(source)"

parseLines1 rule = Parsec.sepEndBy1 rule $ Parsec.char '\n'

parseInt :: Parsec.Parsec String () Int
parseInt = read <$> Parsec.many1 Parsec.digit