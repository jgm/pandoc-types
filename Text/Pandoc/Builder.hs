{-# LANGUAGE TypeSynonymInstances #-}
{-
Copyright (C) 2010 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Builder
   Copyright   : Copyright (C) 2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Some convenience functions for building pandoc documents
programmatically.  Example of use (requires the OverloadedStrings
language extension):

> import Text.Pandoc.Builder
>
> myDoc :: Pandoc
> myDoc = setTitle "My title" $ doc $
>   para "This is the first paragraph" +++
>   para ("And " +++ emph "another" +++ ".") +++
>   bulletList [ plain $ "item one"
>              , plain $ "item two and a " +++
>                  link "/url" "go to url" "link"
>              ]

-}

module Text.Pandoc.Builder ( module Text.Pandoc.Definition
                           , Inlines
                           , Blocks
                           , toList
                           , fromList
                           , empty
                           , (+++)
                           -- * Document builders
                           , doc
                           , setTitle
                           , setAuthors
                           , setDate
                           -- * Inline list builders
                           , text
                           , str
                           , emph
                           , strong
                           , strikeout
                           , superscript
                           , subscript
                           , smallcaps
                           , quoted
                           , cite
                           , code
                           , space
                           , emdash
                           , endash
                           , apostrophe
                           , linebreak
                           , math
                           , tex
                           , htmlInline
                           , link
                           , image
                           , note
                           -- * Block list builders
                           , para
                           , plain
                           , codeBlock
                           , rawHtml
                           , singletonQuote
                           , bulletList
                           , orderedList
                           , definitionList
                           , header
                           , horizontalRule
                           , table
                           )
where
import Text.Pandoc.Definition
import Data.String
import Data.Monoid
import Data.Sequence (Seq, fromList, singleton, empty, (><))
import Data.Foldable (Foldable, toList)
import Data.List (groupBy)
import Data.Char (isSpace)

type Inlines = Seq Inline

-- Foldable gives us toList
-- Monoid gives us mappend, mempty

instance IsString Inlines where
  fromString = text

type Blocks = Seq Block

(+++) :: Monoid a => a -> a -> a
(+++) = mappend

-- Document builders

doc :: Blocks -> Pandoc
doc = Pandoc (Meta [] [] []) . toList

setTitle :: Inlines -> Pandoc -> Pandoc
setTitle t (Pandoc m bs) = Pandoc m{ docTitle = toList t } bs

setAuthors :: [Inlines] -> Pandoc -> Pandoc
setAuthors as (Pandoc m bs) = Pandoc m{ docAuthors = map toList as } bs

setDate :: Inlines -> Pandoc -> Pandoc
setDate d (Pandoc m bs) = Pandoc m{ docDate = toList d } bs

-- Inline list builders

-- | Convert a string to Inlines, treating interword spaces as 'Space's.
-- If you want a 'Str' with literal spaces, use 'rawStr'.
text :: String -> Inlines
text = fromList . (map conv) . breakBySpaces
  where breakBySpaces = groupBy sameCategory
        sameCategory x y = (isSpace x && isSpace y) ||
                           not (isSpace x || isSpace y)
        conv xs | all isSpace xs = Space
        conv xs = Str xs

str :: String -> Inlines
str = singleton . Str

emph :: Inlines -> Inlines
emph = singleton . Emph . toList

strong :: Inlines -> Inlines
strong = singleton . Strong . toList

strikeout :: Inlines -> Inlines
strikeout = singleton . Strikeout . toList

superscript :: Inlines -> Inlines
superscript = singleton . Superscript . toList

subscript :: Inlines -> Inlines
subscript = singleton . Subscript . toList

smallcaps :: Inlines -> Inlines
smallcaps = singleton . SmallCaps . toList

quoted :: QuoteType -> Inlines -> Inlines
quoted qt = singleton . Quoted qt . toList

cite :: [Citation] -> Inlines -> Inlines
cite cts = singleton . Cite cts . toList

code :: String -> Inlines
code = singleton . Code

space :: Inlines
space = singleton Space

emdash :: Inlines
emdash = singleton EmDash

endash :: Inlines
endash = singleton EnDash

apostrophe :: Inlines
apostrophe = singleton Apostrophe

ellipses :: Inlines
ellipses = singleton Ellipses

linebreak :: Inlines
linebreak = singleton LineBreak

math :: MathType -> String -> Inlines
math mt = singleton . Math mt

tex :: String -> Inlines
tex = singleton . TeX

htmlInline :: String -> Inlines
htmlInline = singleton . HtmlInline

link :: String -> String -> Inlines -> Inlines
link url title x = singleton $ Link (toList x) (url, title)

image :: String -> String -> Inlines -> Inlines
image url title x = singleton $ Link (toList x) (url, title)

note :: Blocks -> Inlines
note = singleton . Note . toList

-- Block list builders

para :: Inlines -> Blocks
para = singleton . Para . toList

plain :: Inlines -> Blocks
plain = singleton . Para . toList

codeBlock :: Attr -> String -> Blocks
codeBlock attrs = singleton . CodeBlock attrs

rawHtml :: String -> Blocks
rawHtml = singleton . RawHtml

singletonQuote :: Blocks -> Blocks
singletonQuote = singleton . BlockQuote . toList

orderedList :: Maybe ListAttributes -> [Blocks] -> Blocks
orderedList mbattrs =
  singleton . OrderedList attrs .  map toList
    where attrs = case mbattrs of
                       Nothing  -> (1, DefaultStyle, DefaultDelim)
                       Just a   -> a

bulletList :: [Blocks] -> Blocks
bulletList = singleton . BulletList . map toList

definitionList :: [(Inlines, [Blocks])] -> Blocks
definitionList = singleton . DefinitionList .
  map (\(t,ds) -> (toList t, map toList ds))

header :: Int -> Inlines -> Blocks
header level = singleton . Header level . toList

horizontalRule :: Blocks
horizontalRule = singleton HorizontalRule

table :: Inlines -> [(Alignment, Double)] -> [Blocks] -> [[Blocks]]
      -> Blocks
table caption cellspecs headers rows = singleton $
  Table (toList caption) aligns widths
      (map toList headers) (map (map toList) rows)
   where (aligns, widths) = unzip cellspecs
