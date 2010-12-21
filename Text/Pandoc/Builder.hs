{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
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
programmatically.  Example of use:

> {-# LANGUAGE OverloadedStrings #-}
> import Text.Pandoc.Builder
> 
> myDoc :: Pandoc
> myDoc = setTitle "My title" $ doc $
>   para "This is the first paragraph" +++
>   para ("And" +++ space +++ emph "another" +++ ".") +++
>   bulletList [ plain $ "item one"
>              , plain $ "item two and a" +++ space +++
>                  link "/url" "go to url" "link"
>              ]

-}

module Text.Pandoc.Builder ( module Text.Pandoc.Definition
                           , Inlines
                           , Blocks
                           , (+++)
                           -- * Document builders
                           , doc
                           , setTitle
                           , setAuthors
                           , setDate
                           -- * Inline list builders
                           , inline
                           , str
                           , rawStr
                           , emph
                           , strong
                           , strikeout
                           , superscript
                           , subscript
                           , smallcaps
                           , quoted
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
                           , block
                           , para
                           , plain
                           , codeBlock
                           , rawHtml
                           , blockQuote
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
import Data.DList hiding (map)
import Data.List (intersperse)

newtype Inlines = Inlines { unInlines :: DList Inline }
                  deriving (Monoid)

instance Show Inlines where
  show (Inlines x) = "Inlines (fromList " ++ show (toList x) ++ ")"

newtype Blocks = Blocks { unBlocks :: DList Block }
                 deriving (Monoid)

instance Show Blocks where
  show (Blocks x) = "Blocks (fromList " ++ show (toList x) ++ ")"

instance IsString Inlines where
  fromString = str

(+++) :: Monoid m => m -> m -> m
(+++) = mappend

-- Document builders

doc :: Blocks -> Pandoc
doc = Pandoc (Meta [] [] []) . toList . unBlocks

setTitle :: Inlines -> Pandoc -> Pandoc
setTitle t (Pandoc m bs) = Pandoc m{ docTitle = toList $ unInlines $ t } bs

setAuthors :: [Inlines] -> Pandoc -> Pandoc
setAuthors as (Pandoc m bs) = Pandoc m{ docAuthors = map (toList . unInlines) as } bs

setDate :: Inlines -> Pandoc -> Pandoc
setDate d (Pandoc m bs) = Pandoc m{ docDate = toList $ unInlines $ d } bs

-- Inline list builders

inline :: Inline -> Inlines
inline = Inlines . singleton

-- | Convert a string to Inlines, treating interword spaces as 'Space's.
-- If you want a 'Str' with literal spaces, use rawStr.
str :: String -> Inlines
str = Inlines . fromList . intersperse Space . (map Str) . words

rawStr :: String -> Inlines
rawStr = inline . Str

emph :: Inlines -> Inlines
emph = inline . Emph . toList . unInlines

strong :: Inlines -> Inlines
strong = inline . Strong . toList . unInlines

strikeout :: Inlines -> Inlines
strikeout = inline . Strikeout . toList . unInlines

superscript :: Inlines -> Inlines
superscript = inline . Superscript . toList . unInlines

subscript :: Inlines -> Inlines
subscript = inline . Subscript . toList . unInlines

smallcaps :: Inlines -> Inlines
smallcaps = inline . SmallCaps . toList . unInlines

quoted :: QuoteType -> Inlines -> Inlines
quoted qt = inline . Quoted qt . toList . unInlines

code :: String -> Inlines
code = inline . Code

space :: Inlines
space = inline Space

emdash :: Inlines
emdash = inline EmDash

endash :: Inlines
endash = inline EnDash

apostrophe :: Inlines
apostrophe = inline Apostrophe

ellipses :: Inlines
ellipses = inline Ellipses

linebreak :: Inlines
linebreak = inline LineBreak

math :: MathType -> String -> Inlines
math mt = inline . Math mt

tex :: String -> Inlines
tex = inline . TeX

htmlInline :: String -> Inlines
htmlInline = inline . HtmlInline

link :: String -> String -> Inlines -> Inlines
link url title x = inline $ Link (toList $ unInlines x) (url, title)

image :: String -> String -> Inlines -> Inlines
image url title x = inline $ Link (toList $ unInlines x) (url, title)

note :: Blocks -> Inlines
note = inline . Note . toList . unBlocks

-- Block list builders

block :: Block -> Blocks
block = Blocks . singleton

para :: Inlines -> Blocks
para = block . Para . toList . unInlines

plain :: Inlines -> Blocks
plain = block . Para . toList . unInlines

codeBlock :: Attr -> String -> Blocks
codeBlock attrs = block . CodeBlock attrs

rawHtml :: String -> Blocks
rawHtml = block . RawHtml

blockQuote :: Blocks -> Blocks
blockQuote = block . BlockQuote . toList . unBlocks

orderedList :: Maybe ListAttributes -> [Blocks] -> Blocks
orderedList mbattrs =
  block . OrderedList attrs .  map (toList . unBlocks)
    where attrs = case mbattrs of
                       Nothing  -> (1, DefaultStyle, DefaultDelim)
                       Just a   -> a

bulletList :: [Blocks] -> Blocks
bulletList = block . BulletList . map (toList . unBlocks)

definitionList :: [(Inlines, [Blocks])] -> Blocks
definitionList = block . DefinitionList .
  map (\(t,ds) -> (toList (unInlines t), map (toList . unBlocks) ds))

header :: Int -> Inlines -> Blocks
header level = block . Header level . toList . unInlines

horizontalRule :: Blocks
horizontalRule = block HorizontalRule

table :: Inlines -> [(Alignment, Double)] -> [Blocks] -> [[Blocks]]
      -> Blocks
table caption cellspecs headers rows = block $
  Table (toList $ unInlines caption) aligns widths
      (map (toList . unBlocks) headers) (map (map (toList . unBlocks)) rows)
   where (aligns, widths) = unzip cellspecs
