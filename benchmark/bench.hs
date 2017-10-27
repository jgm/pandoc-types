{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main (bench, defaultMain, nf)
import Data.String.Conversions
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Builder hiding (Pandoc, Meta, MetaValue, Inline, Block, Citation)

type TestStringType = String

-- redefining these so we can easily replace 'String' with another type just for testing.
type Pandoc = Pandoc' TestStringType
type Inline = Inline' TestStringType


main :: IO ()
main = do
  defaultMain [
      bench "simple walk" $ nf (walk prependZeroWidthSpace) mydoc
    , bench "walk concatMap" $ nf (walk $ concatMap prependZeroWidthSpace') mydoc
    , bench "walk lists" $ nf (walk prependZeroWidthSpace'') mydoc
    ]

prependZeroWidthSpace :: Inline -> Inline
prependZeroWidthSpace (Str s) = Str ("\8203" <> s)
prependZeroWidthSpace x = x

prependZeroWidthSpace' :: Inline -> [Inline]
prependZeroWidthSpace' (Str s) = [Str ("\8203" <> s)]
prependZeroWidthSpace' x = [x]

prependZeroWidthSpace'' :: [Inline] -> [Inline]
prependZeroWidthSpace'' (Str s : xs) =
  (Str ("\8203" <> s) : prependZeroWidthSpace'' xs)
prependZeroWidthSpace'' (x : xs) =
  x : prependZeroWidthSpace'' xs
prependZeroWidthSpace'' [] = []

mydoc :: Pandoc
mydoc = setTitle "My title" $ doc $
   mconcat $ replicate 50 $
   para "This is the first paragraph" <>
   para ("And " <> emph "another" <> ".") <>
   bulletList [ para "item one" <> para "continuation"
              , plain ("item two and a " <>
                  link "/url" "go to url" "link")
              ]
