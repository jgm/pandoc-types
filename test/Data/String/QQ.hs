-- | This module is based off the QQ implementation from string-qq
-- (https://github.com/audreyt/string-qq).
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE RankNTypes #-}
module Data.String.QQ (s) where
import Data.String (IsString(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))

s :: QuasiQuoter
s = QuasiQuoter expr pat typ dec
  where
    expr = (\a -> [|fromString a|]) . clean
    pat = error "Cannot use s as a pattern"
    typ = error "Cannot use s as a type"
    dec = error "Cannot use s as a dec"
    clean = removeCarriageReturns . trimLeadingNewline
    removeCarriageReturns = filter (/= '\r')
    trimLeadingNewline ('\n':xs) = xs
    trimLeadingNewline xs = xs
