{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    DeriveDataTypeable, GeneralizedNewtypeDeriving, CPP, StandaloneDeriving,
    DeriveGeneric, DeriveTraversable, FlexibleContexts, OverloadedStrings,
    UndecidableInstances, ScopedTypeVariables #-}
{-
Copyright (C) 2010-2016 John MacFarlane

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of John MacFarlane nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{- |
   Module      : Text.Pandoc.Builder
   Copyright   : Copyright (C) 2010-2016 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Convenience functions for building pandoc documents programmatically.

Example of use (with @OverloadedStrings@ pragma):

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
> import Data.Map (fromList)
>
> myDoc :: Pandoc
> myDoc = Pandoc (Meta {unMeta = fromList [("title",
>           MetaInlines [Str "My",Space,Str "title"])]})
>         [Para [Str "This",Space,Str "is",Space,Str "the",Space,Str "first",
>          Space,Str "paragraph"],Para [Str "And",Space,Emph [Str "another"],
>          Str "."]
>         ,BulletList [
>           [Para [Str "item",Space,Str "one"]
>           ,Para [Str "continuation"]]
>          ,[Plain [Str "item",Space,Str "two",Space,Str "and",Space,
>                   Str "a",Space,Link nullAttr [Str "link"] ("/url","go to url")]]]]

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
                           , Many(..)
                           , Inlines
                           , Inlines'
                           , Blocks
                           , Blocks'
                           , (<>)
                           , singleton
                           , toList
                           , fromList
                           , isNull
                           -- * Document builders
                           , doc
                           , ToMetaValue(..)
                           , HasMeta(..)
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
                           , softbreak
                           , linebreak
                           , math
                           , displayMath
                           , rawInline
                           , link
                           , linkWith
                           , image
                           , imageWith
                           , note
                           , spanWith
                           , trimInlines
                           -- * Block list builders
                           , para
                           , plain
                           , lineBlock
                           , codeBlockWith
                           , codeBlock
                           , rawBlock
                           , blockQuote
                           , bulletList
                           , orderedListWith
                           , orderedList
                           , definitionList
                           , header
                           , headerWith
                           , horizontalRule
                           , table
                           , simpleTable
                           , divWith
                           )
where
import Text.Pandoc.Definition
import Data.String
import Data.String.Conversions
import qualified Data.Map as M
import qualified Data.Text.Lazy as LT
import Data.Sequence (Seq, (|>), viewr, viewl, ViewR(..), ViewL(..))
import qualified Data.Sequence as Seq
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Data
import Control.Arrow ((***))
import GHC.Generics (Generic)

type PandocString = LT

#if MIN_VERSION_base(4,5,0)
-- (<>) is defined in Data.Monoid
#else
infixr 6 <>

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

newtype Many a = Many { unMany :: Seq a }
                 deriving (Data, Ord, Eq, Typeable, Foldable, Traversable, Functor, Show, Read)

deriving instance Generic (Many a)

toList :: Many a -> [a]
toList = F.toList

singleton :: a -> Many a
singleton = Many . Seq.singleton

fromList :: [a] -> Many a
fromList = Many . Seq.fromList

isNull :: Many a -> Bool
isNull = Seq.null . unMany

type Inlines = Inlines' String
type Blocks  = Blocks' String

type Inlines' string = Many (Inline' string)
type Blocks' string  = Many (Block' string)

deriving instance Monoid string => Monoid (Blocks' string)

instance Monoid string => Monoid (Inlines' string) where
  mempty = Many mempty
  (Many xs) `mappend` (Many ys) =
    case (viewr xs, viewl ys) of
      (EmptyR, _) -> Many ys
      (_, EmptyL) -> Many xs
      (xs' :> x, y :< ys') -> Many (meld `mappend` ys')
        where meld = case (x, y) of
                          (Space, Space)     -> xs' |> Space
                          (Space, SoftBreak) -> xs' |> SoftBreak
                          (SoftBreak, Space) -> xs' |> SoftBreak
                          (Str t1, Str t2)   -> xs' |> Str (t1 <> t2)
                          (Emph i1, Emph i2) -> xs' |> Emph (i1 <> i2)
                          (Strong i1, Strong i2) -> xs' |> Strong (i1 <> i2)
                          (Subscript i1, Subscript i2) -> xs' |> Subscript (i1 <> i2)
                          (Superscript i1, Superscript i2) -> xs' |> Superscript (i1 <> i2)
                          (Strikeout i1, Strikeout i2) -> xs' |> Strikeout (i1 <> i2)
                          (Space, LineBreak) -> xs' |> LineBreak
                          (LineBreak, Space) -> xs' |> LineBreak
                          (SoftBreak, LineBreak) -> xs' |> LineBreak
                          (LineBreak, SoftBreak) -> xs' |> LineBreak
                          (SoftBreak, SoftBreak) -> xs' |> SoftBreak
                          _                  -> xs' |> x |> y

instance ConvertibleStrings PandocString string => IsString (Inlines' string) where
   fromString = text

-- | Trim leading and trailing spaces and softbreaks from an Inlines.
trimInlines :: Inlines' string -> Inlines' string
#if MIN_VERSION_containers(0,4,0)
trimInlines (Many ils) = Many $ Seq.dropWhileL isSp $
                            Seq.dropWhileR isSp $ ils
#else
-- for GHC 6.12, we need to workaround a bug in dropWhileR
-- see http://hackage.haskell.org/trac/ghc/ticket/4157
trimInlines (Many ils) = Many $ Seq.dropWhileL isSp
                            Seq.reverse $ Seq.dropWhileL isSp $
                            Seq.reverse $ ils
#endif
  where isSp Space = True
        isSp SoftBreak = True
        isSp _ = False

-- Document builders

doc :: Ord string => Blocks' string -> Pandoc' string
doc = Pandoc nullMeta . toList

class ToMetaValue a string where
  toMetaValue :: a -> MetaValue' string

instance ToMetaValue (MetaValue' string) string where
  toMetaValue = id

instance ToMetaValue (Blocks' string) string where
  toMetaValue = MetaBlocks . toList

instance ToMetaValue (Inlines' string) string where
  toMetaValue = MetaInlines . toList

instance ToMetaValue Bool string where
  toMetaValue = MetaBool

instance ToMetaValue a string => ToMetaValue [a] string where
  toMetaValue = MetaList . map toMetaValue

instance ToMetaValue a string => ToMetaValue (M.Map string a) string where
  toMetaValue = MetaMap . M.map toMetaValue

class HasMeta a string where
  setMeta :: ToMetaValue b string => string -> b -> a string -> a string
  deleteMeta :: string -> a string -> a string

instance Ord string => HasMeta Meta' string where
  setMeta key val (Meta ms) = Meta $ M.insert key (toMetaValue val) ms
  deleteMeta key (Meta ms) = Meta $ M.delete key ms

instance Ord string => HasMeta Pandoc' string where
  setMeta key val (Pandoc (Meta ms) bs) =
    Pandoc (Meta $ M.insert key (toMetaValue val) ms) bs
  deleteMeta key (Pandoc (Meta ms) bs) =
    Pandoc (Meta $ M.delete key ms) bs

setTitle :: (IsString string, Ord string) => Inlines' string -> Pandoc' string -> Pandoc' string
setTitle = setMeta "title"

setAuthors :: (IsString string, Ord string) => [Inlines' string] -> Pandoc' string -> Pandoc' string
setAuthors = setMeta "author"

setDate :: (IsString string, Ord string) => Inlines' string -> Pandoc' string -> Pandoc' string
setDate = setMeta "date"

-- Inline list builders

-- | Convert a 'String' to 'Inlines', treating interword spaces as 'Space's
-- or 'SoftBreak's.  If you want a 'Str' with literal spaces, use 'str'.
text :: forall string string'.
        (ConvertibleStrings string PandocString, ConvertibleStrings PandocString string')
     => string -> Inlines' string'
text = fromList . map conv . breakBySpaces . cs
  where breakBySpaces :: LT -> [LT]
        breakBySpaces = LT.groupBy sameCategory

        sameCategory :: Char -> Char -> Bool
        sameCategory x y = (is_space x && is_space y) ||
                           (not $ is_space x || is_space y)

        conv :: PandocString -> Inline' string'
        conv xs | LT.all is_space xs =
           if LT.any is_newline xs
              then SoftBreak
              else Space
        conv xs = Str $ cs xs
        is_space ' '    = True
        is_space '\r'   = True
        is_space '\n'   = True
        is_space '\t'   = True
        is_space _      = False
        is_newline '\r' = True
        is_newline '\n' = True
        is_newline _    = False

str :: string -> Inlines' string
str = singleton . Str

emph :: Inlines' string -> Inlines' string
emph = singleton . Emph . toList

strong :: Inlines' string -> Inlines' string
strong = singleton . Strong . toList

strikeout :: Inlines' string -> Inlines' string
strikeout = singleton . Strikeout . toList

superscript :: Inlines' string -> Inlines' string
superscript = singleton . Superscript . toList

subscript :: Inlines' string -> Inlines' string
subscript = singleton . Subscript . toList

smallcaps :: Inlines' string -> Inlines' string
smallcaps = singleton . SmallCaps . toList

singleQuoted :: Inlines' string -> Inlines' string
singleQuoted = quoted SingleQuote

doubleQuoted :: Inlines' string -> Inlines' string
doubleQuoted = quoted DoubleQuote

quoted :: QuoteType -> Inlines' string -> Inlines' string
quoted qt = singleton . Quoted qt . toList

cite :: [Citation' string] -> Inlines' string -> Inlines' string
cite cts = singleton . Cite cts . toList

-- | Inline code with attributes.
codeWith :: Attr' string -> string -> Inlines' string
codeWith attrs = singleton . Code attrs

-- | Plain inline code.
code :: IsString string => string -> Inlines' string
code = codeWith nullAttr

space :: Inlines' string
space = singleton Space

softbreak :: Inlines' string
softbreak = singleton SoftBreak

linebreak :: Inlines' string
linebreak = singleton LineBreak

-- | Inline math
math :: string -> Inlines' string
math = singleton . Math InlineMath

-- | Display math
displayMath :: string -> Inlines' string
displayMath = singleton . Math DisplayMath

rawInline :: String -> string -> Inlines' string
rawInline format = singleton . RawInline (Format format)

link :: IsString string
     => string  -- ^ URL
     -> string  -- ^ Title
     -> Inlines' string -- ^ Label
     -> Inlines' string
link = linkWith nullAttr

linkWith :: Attr' string    -- ^ Attributes
         -> string  -- ^ URL
         -> string  -- ^ Title
         -> Inlines' string -- ^ Label
         -> Inlines' string
linkWith attr url title x = singleton $ Link attr (toList x) (url, title)

image :: IsString string
      => string  -- ^ URL
      -> string  -- ^ Title
      -> Inlines' string -- ^ Alt text
      -> Inlines' string
image = imageWith nullAttr

imageWith :: Attr' string -- ^ Attributes
          -> string  -- ^ URL
          -> string  -- ^ Title
          -> Inlines' string -- ^ Alt text
          -> Inlines' string
imageWith attr url title x = singleton $ Image attr (toList x) (url, title)

note :: Blocks' string -> Inlines' string
note = singleton . Note . toList

spanWith :: Attr' string -> Inlines' string -> Inlines' string
spanWith attr = singleton . Span attr . toList

-- Block list builders

para :: Inlines' string -> Blocks' string
para = singleton . Para . toList

plain :: Monoid string => Inlines' string -> Blocks' string
plain ils = if isNull ils
               then mempty
               else singleton . Plain . toList $ ils

lineBlock :: [Inlines' string] -> Blocks' string
lineBlock = singleton . LineBlock . map toList

-- | A code block with attributes.
codeBlockWith :: Attr' string -> string -> Blocks' string
codeBlockWith attrs = singleton . CodeBlock attrs

-- | A plain code block.
codeBlock :: IsString string => string -> Blocks' string
codeBlock = codeBlockWith nullAttr

rawBlock :: String -> string' -> Blocks' string'
rawBlock format = singleton . RawBlock (Format format)

blockQuote :: Blocks' string -> Blocks' string
blockQuote = singleton . BlockQuote . toList

-- | Ordered list with attributes.
orderedListWith :: ListAttributes -> [Blocks' string] -> Blocks' string
orderedListWith attrs = singleton . OrderedList attrs .  map toList

-- | Ordered list with default attributes.
orderedList :: [Blocks' string] -> Blocks' string
orderedList = orderedListWith (1, DefaultStyle, DefaultDelim)

bulletList :: [Blocks' string] -> Blocks' string
bulletList = singleton . BulletList . map toList

definitionList :: [(Inlines' string, [Blocks' string])] -> Blocks' string
definitionList = singleton . DefinitionList .  map (toList *** map toList)

header :: IsString string
       => Int  -- ^ Level
       -> Inlines' string
       -> Blocks' string
header = headerWith nullAttr

headerWith :: Attr' string -> Int -> Inlines' string -> Blocks' string
headerWith attr level = singleton . Header level attr . toList

horizontalRule :: Blocks' string
horizontalRule = singleton HorizontalRule

table :: Monoid string
      => Inlines' string       -- ^ Caption
      -> [(Alignment, Double)] -- ^ Column alignments and fractional widths
      -> [Blocks' string]      -- ^ Headers
      -> [[Blocks' string]]    -- ^ Rows
      -> Blocks' string
table caption cellspecs headers rows = singleton $
  Table (toList caption) aligns widths
      (map toList headers') (map (map toList) rows)
   where (aligns, widths) = unzip cellspecs
         numcols  = case (headers:rows) of
                         [] -> 0
                         xs -> maximum (map length xs)
         headers' = if null headers
                       then replicate numcols mempty
                       else headers

-- | A simple table without a caption.
simpleTable :: Monoid string
            => [Blocks' string]   -- ^ Headers
            -> [[Blocks' string]] -- ^ Rows
            -> Blocks' string
simpleTable headers rows =
  table mempty (replicate numcols defaults) headers rows
  where defaults = (AlignDefault, 0)
        numcols  = case (headers:rows) of
                        [] -> 0
                        xs -> maximum (map length xs)

divWith :: Attr' string -> Blocks' string -> Blocks' string
divWith attr = singleton . Div attr . toList
