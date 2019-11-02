{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable,
    GeneralizedNewtypeDeriving, CPP, StandaloneDeriving, DeriveGeneric,
    DeriveTraversable, OverloadedStrings #-}
{-
Copyright (C) 2010-2019 John MacFarlane

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
   Copyright   : Copyright (C) 2010-2019 John MacFarlane
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
                           , Blocks
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
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq, (|>), viewr, viewl, ViewR(..), ViewL(..))
import qualified Data.Sequence as Seq
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Data
import Control.Arrow ((***))
import GHC.Generics (Generic)
import Data.Semigroup (Semigroup(..))

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

type Inlines = Many Inline
type Blocks  = Many Block

deriving instance Semigroup Blocks
deriving instance Monoid Blocks

instance Semigroup Inlines where
  (Many xs) <> (Many ys) =
    case (viewr xs, viewl ys) of
      (EmptyR, _) -> Many ys
      (_, EmptyL) -> Many xs
      (xs' :> x, y :< ys') -> Many (meld <> ys')
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
instance Monoid Inlines where
  mempty = Many mempty
  mappend = (<>)

instance IsString Inlines where
   fromString = text . T.pack

-- | Trim leading and trailing spaces and softbreaks from an Inlines.
trimInlines :: Inlines -> Inlines
#if MIN_VERSION_containers(0,4,0)
trimInlines (Many ils) = Many $ Seq.dropWhileL isSp $
                            Seq.dropWhileR isSp $ ils
#else
-- for GHC 6.12, we need to workaround a bug in dropWhileR
-- see http://hackage.haskell.org/trac/ghc/ticket/4157
trimInlines (Many ils) = Many $ Seq.dropWhileL isSp $
                            Seq.reverse $ Seq.dropWhileL isSp $
                            Seq.reverse ils
#endif
  where isSp Space = True
        isSp SoftBreak = True
        isSp _ = False

-- Document builders

doc :: Blocks -> Pandoc
doc = Pandoc nullMeta . toList

class ToMetaValue a where
  toMetaValue :: a -> MetaValue

instance ToMetaValue MetaValue where
  toMetaValue = id

instance ToMetaValue Blocks where
  toMetaValue = MetaBlocks . toList

instance ToMetaValue Inlines where
  toMetaValue = MetaInlines . toList

instance ToMetaValue Bool where
  toMetaValue = MetaBool

instance ToMetaValue Text where
  toMetaValue = MetaString

instance {-# OVERLAPPING #-} ToMetaValue String where
  toMetaValue = MetaString . T.pack

instance ToMetaValue a => ToMetaValue [a] where
  toMetaValue = MetaList . map toMetaValue

instance ToMetaValue a => ToMetaValue (M.Map Text a) where
  toMetaValue = MetaMap . M.map toMetaValue

instance ToMetaValue a => ToMetaValue (M.Map String a) where
  toMetaValue = MetaMap . M.map toMetaValue . M.mapKeys T.pack

class HasMeta a where
  setMeta :: ToMetaValue b => Text -> b -> a -> a
  deleteMeta :: Text -> a -> a

instance HasMeta Meta where
  setMeta key val (Meta ms) = Meta $ M.insert key (toMetaValue val) ms
  deleteMeta key (Meta ms) = Meta $ M.delete key ms

instance HasMeta Pandoc where
  setMeta key val (Pandoc (Meta ms) bs) =
    Pandoc (Meta $ M.insert key (toMetaValue val) ms) bs
  deleteMeta key (Pandoc (Meta ms) bs) =
    Pandoc (Meta $ M.delete key ms) bs

setTitle :: Inlines -> Pandoc -> Pandoc
setTitle = setMeta "title"

setAuthors :: [Inlines] -> Pandoc -> Pandoc
setAuthors = setMeta "author"

setDate :: Inlines -> Pandoc -> Pandoc
setDate = setMeta "date"

-- Inline list builders

-- | Convert a 'String' to 'Inlines', treating interword spaces as 'Space's
-- or 'SoftBreak's.  If you want a 'Str' with literal spaces, use 'str'.
text :: Text -> Inlines
text = fromList . map conv . breakBySpaces
  where breakBySpaces = T.groupBy sameCategory
        sameCategory x y = is_space x == is_space y
        conv xs | T.all is_space xs =
           if T.any is_newline xs
              then SoftBreak
              else Space
        conv xs = Str xs
        is_space ' '    = True
        is_space '\r'   = True
        is_space '\n'   = True
        is_space '\t'   = True
        is_space _      = False
        is_newline '\r' = True
        is_newline '\n' = True
        is_newline _    = False

str :: Text -> Inlines
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
codeWith :: Attr -> Text -> Inlines
codeWith attrs = singleton . Code attrs

-- | Plain inline code.
code :: Text -> Inlines
code = codeWith nullAttr

space :: Inlines
space = singleton Space

softbreak :: Inlines
softbreak = singleton SoftBreak

linebreak :: Inlines
linebreak = singleton LineBreak

-- | Inline math
math :: Text -> Inlines
math = singleton . Math InlineMath

-- | Display math
displayMath :: Text -> Inlines
displayMath = singleton . Math DisplayMath

rawInline :: Text -> Text -> Inlines
rawInline format = singleton . RawInline (Format format)

link :: Text  -- ^ URL
     -> Text  -- ^ Title
     -> Inlines -- ^ Label
     -> Inlines
link = linkWith nullAttr

linkWith :: Attr    -- ^ Attributes
         -> Text  -- ^ URL
         -> Text  -- ^ Title
         -> Inlines -- ^ Label
         -> Inlines
linkWith attr url title x = singleton $ Link attr (toList x) (url, title)

image :: Text  -- ^ URL
      -> Text  -- ^ Title
      -> Inlines -- ^ Alt text
      -> Inlines
image = imageWith nullAttr

imageWith :: Attr -- ^ Attributes
          -> Text  -- ^ URL
          -> Text  -- ^ Title
          -> Inlines -- ^ Alt text
          -> Inlines
imageWith attr url title x = singleton $ Image attr (toList x) (url, title)

note :: Blocks -> Inlines
note = singleton . Note . toList

spanWith :: Attr -> Inlines -> Inlines
spanWith attr = singleton . Span attr . toList

-- Block list builders

para :: Inlines -> Blocks
para = singleton . Para . toList

plain :: Inlines -> Blocks
plain ils = if isNull ils
               then mempty
               else singleton . Plain . toList $ ils

lineBlock :: [Inlines] -> Blocks
lineBlock = singleton . LineBlock . map toList

-- | A code block with attributes.
codeBlockWith :: Attr -> Text -> Blocks
codeBlockWith attrs = singleton . CodeBlock attrs

-- | A plain code block.
codeBlock :: Text -> Blocks
codeBlock = codeBlockWith nullAttr

rawBlock :: Text -> Text -> Blocks
rawBlock format = singleton . RawBlock (Format format)

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
header = headerWith nullAttr

headerWith :: Attr -> Int -> Inlines -> Blocks
headerWith attr level = singleton . Header level attr . toList

horizontalRule :: Blocks
horizontalRule = singleton HorizontalRule

-- | Table builder. Rows and headers will be padded or truncated to the size of
-- @cellspecs@
table :: Inlines               -- ^ Caption
      -> [(Alignment, Double)] -- ^ Column alignments and fractional widths
      -> [Blocks]              -- ^ Headers
      -> [[Blocks]]            -- ^ Rows
      -> Blocks
table caption cellspecs headers rows = singleton $
  Table (toList caption) aligns widths (sanitise headers) (map sanitise rows)
   where (aligns, widths) = unzip cellspecs
         sanitise = map toList . pad mempty numcols
         numcols = length cellspecs
         pad element upTo list = take upTo (list ++ repeat element)

-- | A simple table without a caption.
simpleTable :: [Blocks]   -- ^ Headers
            -> [[Blocks]] -- ^ Rows
            -> Blocks
simpleTable headers rows =
  table mempty (replicate numcols defaults) headers rows
  where defaults = (AlignDefault, 0)
        numcols  = case headers:rows of
                        [] -> 0
                        xs -> maximum (map length xs)

divWith :: Attr -> Blocks -> Blocks
divWith attr = singleton . Div attr . toList
