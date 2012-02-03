{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
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

Convenience functions for building pandoc documents programmatically.

Example of use:

> {-# LANGUAGE OverloadedStrings #-}
> import Text.Pandoc.Builder
>
> myDoc :: Pandoc
> myDoc = setTitle "My title" $ doc $
>   para "This is the first paragraph" <>
>   para ("And " <> emph "another" <> ".") <>
>   bulletList [ para "item one" <> para "continuation"
>              , plain ("item two and a " <>
>                  link "/url" "go to url" "link")
>              ]

Isn't that nicer than writing the following?

> import Text.Pandoc.Definition
>
> myDoc :: Pandoc
> myDoc = Pandoc (Meta {docTitle = [Str "My",Space,Str "title"]
>                      , docAuthors = []
>                      , docDate = []})
>  [Para [Str "This",Space,Str "is",Space,Str "the",Space,Str "first",
>   Space,Str "paragraph"]
>  ,Para [Str "And",Space,Emph [Str "another"],Str "."]
>  ,BulletList [[Para [Str "item",Space,Str "one"]
>               ,Para [Str "continuation"]]
>              ,[Plain [Str "item",Space,Str "two",Space,Str "and", Space,
>                 Str "a",Space,Link [Str "link"] ("/url","go to url")]]]]

And of course, you can use Haskell to define your own builders:

> import Text.Pandoc.Builder
> import Text.JSON
> import Control.Arrow ((***))
> import Data.Monoid (mempty)
>
> -- | Converts a JSON document into 'Blocks'.
> json :: String -> Blocks
> json x =
>   case decode x of
>        Ok y    -> jsValueToBlocks y
>        Error y -> error y
>    where jsValueToBlocks x =
>           case x of
>            JSNull         -> mempty
>            JSBool x       -> plain $ text $ show x
>            JSRational _ x -> plain $ text $ show x
>            JSString x     -> plain $ text $ fromJSString x
>            JSArray xs     -> bulletList $ map jsValueToBlocks xs
>            JSObject x     -> definitionList $
>                               map (text *** (:[]) . jsValueToBlocks) $
>                               fromJSObject x

-}

module Text.Pandoc.Builder ( module Text.Pandoc.Definition
                           , Inlines(..)
                           , Blocks(..)
                           , (<>)
                           , Listable(..)
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
                           , singleQuoted
                           , doubleQuoted
                           , cite
                           , codeWith
                           , code
                           , space
                           , linebreak
                           , math
                           , displayMath
                           , rawInline
                           , link
                           , image
                           , note
                           , trimInlines
                           -- * Block list builders
                           , para
                           , plain
                           , codeBlockWith
                           , codeBlock
                           , rawBlock
                           , blockQuote
                           , bulletList
                           , orderedListWith
                           , orderedList
                           , definitionList
                           , header
                           , horizontalRule
                           , table
                           , simpleTable
                           )
where
import Text.Pandoc.Definition
import Data.String
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (|>), viewr, viewl, ViewR(..), ViewL(..))
import qualified Data.Sequence as Seq
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.List (groupBy, intersperse)
import Data.Data
import Data.Typeable
import Control.Arrow ((***))

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

newtype Inlines = Inlines { unInlines :: Seq Inline }
                deriving (Data, Ord, Eq, Typeable)

-- We show an Inlines just like [Inline].
instance Show Inlines where
  show = show . F.toList . unInlines

instance Read Inlines where
  readsPrec n = map (\(x,y) -> (Inlines . Seq.fromList $ x, y)) . readsPrec n

instance Monoid Inlines where
  mempty = Inlines mempty
  (Inlines xs) `mappend` (Inlines ys) =
    case (viewr xs, viewl ys) of
      (EmptyR, _) -> Inlines ys
      (_, EmptyL) -> Inlines xs
      (xs' :> x, y :< ys') -> Inlines (meld `mappend` ys')
        where meld = case (x, y) of
                          (Space, Space)     -> xs' |> Space
                          (Str t1, Str t2)   -> xs' |> Str (t1 <> t2)
                          (Emph i1, Emph i2) -> xs' |> Emph (i1 <> i2)
                          (Strong i1, Strong i2) -> xs' |> Strong (i1 <> i2)
                          (Subscript i1, Subscript i2) -> xs' |> Subscript (i1 <> i2)
                          (Superscript i1, Superscript i2) -> xs' |> Superscript (i1 <> i2)
                          (Strikeout i1, Strikeout i2) -> xs' |> Strikeout (i1 <> i2)
                          (Space, LineBreak) -> xs' |> LineBreak
                          _                  -> xs' |> x |> y

instance IsString Inlines where
  fromString = text

newtype Blocks = Blocks { unBlocks :: Seq Block }
                deriving (Data, Ord, Eq, Typeable, Monoid)

-- We show a Blocks just like [Block].
instance Show Blocks where
  show = show . F.toList . unBlocks

instance Read Blocks where
  readsPrec n = map (\(x,y) -> (Blocks . Seq.fromList $ x, y)) . readsPrec n

class Listable a b where
  toList     :: a -> [b]
  fromList   :: [b] -> a
  foldMap    :: (b -> a) -> a -> a
  singleton  :: b -> a
  foldlM     :: Monad m => (a -> b -> m a) -> a -> a -> m a
  isNull     :: a -> Bool

instance Listable Inlines Inline where
  toList         = F.toList . unInlines
  fromList       = Inlines . Seq.fromList
  foldMap f      = F.foldMap f . unInlines
  singleton      = Inlines . Seq.singleton
  foldlM f x     = F.foldlM f x . unInlines
  isNull         = Seq.null . unInlines

instance Listable Blocks Block where
  toList         = F.toList . unBlocks
  fromList       = Blocks . Seq.fromList
  foldMap  f     = F.foldMap f . unBlocks
  singleton      = Blocks . Seq.singleton
  foldlM f x     = F.foldlM f x . unBlocks
  isNull         = Seq.null . unBlocks

-- | Trim leading and trailing Sp (spaces) from an Inlines.
trimInlines :: Inlines -> Inlines
trimInlines (Inlines ils) = Inlines $ Seq.dropWhileL (== Space) $
                            Seq.dropWhileR (== Space) $ ils

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

-- | Convert a 'String' to 'Inlines', treating interword spaces as 'Space's.
-- If you want a 'Str' with literal spaces, use 'str'.
text :: String -> Inlines
text = fromList . map conv . breakBySpaces
  where breakBySpaces = groupBy sameCategory
        sameCategory x y = (is_space x && is_space y) ||
                           (not $ is_space x || is_space y)
        conv xs | all is_space xs = Space
        conv xs = Str xs
        is_space ' '  = True
        is_space '\n' = True
        is_space '\t' = True
        is_space _    = False

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

singleQuoted :: Inlines -> Inlines
singleQuoted = quoted SingleQuote

doubleQuoted :: Inlines -> Inlines
doubleQuoted = quoted DoubleQuote

quoted :: QuoteType -> Inlines -> Inlines
quoted qt = singleton . Quoted qt . toList

cite :: [Citation] -> Inlines -> Inlines
cite cts = singleton . Cite cts . toList

-- | Inline code with attributes.
codeWith :: Attr -> String -> Inlines
codeWith attrs = singleton . Code attrs

-- | Plain inline code.
code :: String -> Inlines
code = codeWith nullAttr

space :: Inlines
space = singleton Space

linebreak :: Inlines
linebreak = singleton LineBreak

-- | Inline math
math :: String -> Inlines
math = singleton . Math InlineMath

-- | Display math
displayMath :: String -> Inlines
displayMath = singleton . Math DisplayMath

rawInline :: Format -> String -> Inlines
rawInline format = singleton . RawInline format

link :: String  -- ^ URL
     -> String  -- ^ Title
     -> Inlines -- ^ Label
     -> Inlines
link url title x = singleton $ Link (toList x) (url, title)

image :: String  -- ^ URL
      -> String  -- ^ Title
      -> Inlines -- ^ Alt text
      -> Inlines
image url title x = singleton $ Image (toList x) (url, title)

note :: Blocks -> Inlines
note = singleton . Note . toList

-- Block list builders

para :: Inlines -> Blocks
para = singleton . Para . toList

plain :: Inlines -> Blocks
plain = singleton . Plain . toList

-- | A code block with attributes.
codeBlockWith :: Attr -> String -> Blocks
codeBlockWith attrs = singleton . CodeBlock attrs

-- | A plain code block.
codeBlock :: String -> Blocks
codeBlock = codeBlockWith nullAttr

rawBlock :: Format -> String -> Blocks
rawBlock format = singleton . RawBlock format

blockQuote :: Blocks -> Blocks
blockQuote = singleton . BlockQuote . toList

-- | Ordered list with attributes.
orderedListWith :: ListAttributes -> [Blocks] -> Blocks
orderedListWith attrs = singleton . OrderedList attrs .  map toList

-- | Ordered list with default attributes.
orderedList :: [Blocks] -> Blocks
orderedList = orderedListWith (1, DefaultStyle, DefaultDelim)

bulletList :: [Blocks] -> Blocks
bulletList = singleton . BulletList . map toList

definitionList :: [(Inlines, [Blocks])] -> Blocks
definitionList = singleton . DefinitionList .  map (toList *** map toList)

header :: Int  -- ^ Level
       -> Inlines
       -> Blocks
header level = singleton . Header level . toList

horizontalRule :: Blocks
horizontalRule = singleton HorizontalRule

table :: Inlines               -- ^ Caption
      -> [(Alignment, Double)] -- ^ Column alignments and fractional widths
      -> [Blocks]              -- ^ Headers
      -> [[Blocks]]            -- ^ Rows
      -> Blocks
table caption cellspecs headers rows = singleton $
  Table (toList caption) aligns widths
      (map toList headers) (map (map toList) rows)
   where (aligns, widths) = unzip cellspecs

-- | A simple table without a caption.
simpleTable :: [Blocks]   -- ^ Headers
            -> [[Blocks]] -- ^ Rows
            -> Blocks
simpleTable headers = table mempty (mapConst defaults headers) headers
  where defaults = (AlignDefault, 0)

mapConst :: Functor f => b -> f a -> f b
mapConst = fmap . const
