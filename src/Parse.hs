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
import Data.Either.Combinators (fromRight')

parse rule = P.parse rule "(source)"

unsafeParse rule = fromRight' . parse rule

parseLines1 rule = sepEndBy1 rule P.newline

parseInt :: P.Parsec String () Int
parseInt = read <$> liftA2 (:) (option ' ' (char '-')) (many1 digit)

