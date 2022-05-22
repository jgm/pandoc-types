{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric,
    FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards, CPP,
    TemplateHaskell , PatternSynonyms, ViewPatterns, StrictData,
    DeriveTraversable
  #-}

{-
Copyright (c) 2006-2019, John MacFarlane

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
   Module      : Text.Pandoc.Definition
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definition of 'Pandoc' data structure for format-neutral representation
of documents.
-}
module Text.Pandoc.Definition.Functors
  ( MetaValueF(..)
  , BlockF(..)
  , pattern SimpleFigure
  , InlineF(..)
  , ListAttributes
  , ListNumberStyle(..)
  , ListNumberDelim(..)
  , Format(..)
  , Attr
  , nullAttr
  , CaptionF(..)
  , ShortCaptionF
  , RowHeadColumns(..)
  , Alignment(..)
  , ColWidth(..)
  , ColSpec
  , RowF(..)
  , TableHeadF(..)
  , TableBodyF(..)
  , TableFootF(..)
  , CellF(..)
  , RowSpan(..)
  , ColSpan(..)
  , QuoteType(..)
  , Target
  , MathType(..)
  , CitationF(..)
  , CitationMode(..)
  , pandocTypesVersion
  ) where

import Data.Generics (Data, Typeable)
import Data.Ord (comparing)
import Data.Aeson hiding (Null)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.String
import Control.DeepSeq
import Paths_pandoc_types (version)
import Data.Version (Version)
import Data.Semigroup ((<>))
import Control.Arrow (second)

data MetaValueF inline block metaValue
               = MetaMap (M.Map Text metaValue)
               | MetaList [metaValue]
               | MetaBool Bool
               | MetaString Text
               | MetaInlines [inline]
               | MetaBlocks [block]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | List attributes.  The first element of the triple is the
-- start number of the list.
type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)

-- | Style of list numbers.
data ListNumberStyle = DefaultStyle
                     | Example
                     | Decimal
                     | LowerRoman
                     | UpperRoman
                     | LowerAlpha
                     | UpperAlpha deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Delimiter of list numbers.
data ListNumberDelim = DefaultDelim
                     | Period
                     | OneParen
                     | TwoParens deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Attributes: identifier, classes, key-value pairs
type Attr = (Text, [Text], [(Text, Text)])

nullAttr :: Attr
nullAttr = ("",[],[])

-- | Formats for raw blocks
newtype Format = Format Text
               deriving (Read, Show, Typeable, Data, Generic, ToJSON, FromJSON)

instance IsString Format where
  fromString f = Format $ T.toCaseFold $ T.pack f

instance Eq Format where
  Format x == Format y = T.toCaseFold x == T.toCaseFold y

instance Ord Format where
  compare (Format x) (Format y) = compare (T.toCaseFold x) (T.toCaseFold y)

-- | The number of columns taken up by the row head of each row of a
-- 'TableBody'. The row body takes up the remaining columns.
newtype RowHeadColumns = RowHeadColumns Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | Alignment of a table column.
data Alignment = AlignLeft
               | AlignRight
               | AlignCenter
               | AlignDefault deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The width of a table column, as a percentage of the text width.
data ColWidth = ColWidth Double
              | ColWidthDefault deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The specification for a single table column.
type ColSpec = (Alignment, ColWidth)

-- | A table row.
data RowF block = Row Attr [CellF block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | The head of a table.
data TableHeadF block = TableHead Attr [RowF block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | A body of a table, with an intermediate head, intermediate body,
-- and the specified number of row header columns in the intermediate
-- body.
data TableBodyF block = TableBody Attr RowHeadColumns [RowF block] [RowF block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | The foot of a table.
data TableFootF block = TableFoot Attr [RowF block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | A short caption, for use in, for instance, lists of figures.
type ShortCaptionF inline = [inline]

-- | The caption of a table, with an optional short caption.
data CaptionF inline block = Caption (Maybe (ShortCaptionF inline)) [block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | A table cell.
data CellF block = Cell Attr Alignment RowSpan ColSpan [block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | The number of rows occupied by a cell; the height of a cell.
newtype RowSpan = RowSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | The number of columns occupied by a cell; the width of a cell.
newtype ColSpan = ColSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | Block element
data BlockF inline block
    -- | Plain text, not a paragraph
    = Plain [inline]
    -- | Paragraph
    | Para [inline]
    -- | Multiple non-breaking lines
    | LineBlock [[inline]]
    -- | Code block (literal) with attributes
    | CodeBlock Attr Text
    -- | Raw block
    | RawBlock Format Text
    -- | Block quote (list of blocks)
    | BlockQuote [block]
    -- | Ordered list (attributes and a list of items, each a list of
    -- blocks)
    | OrderedList ListAttributes [[block]]
    -- | Bullet list (list of items, each a list of blocks)
    | BulletList [[block]]
    -- | Definition list. Each list item is a pair consisting of a
    -- term (a list of inlines) and one or more definitions (each a
    -- list of blocks)
    | DefinitionList [([inline],[[block]])]
    -- | Header - level (integer) and text (inlines)
    | Header Int Attr [inline]
    -- | Horizontal rule
    | HorizontalRule
    -- | Table, with attributes, caption, optional short caption,
    -- column alignments and widths (required), table head, table
    -- bodies, and table foot
    | Table Attr (CaptionF inline block) [ColSpec] (TableHeadF block) [TableBodyF block] (TableFootF block)
    -- | Generic block container with attributes
    | Div Attr [block]
    -- | Nothing
    | Null
    deriving ( Eq, Ord, Read, Show, Typeable, Data, Generic
             , Functor, Foldable, Traversable
             )

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Link target (URL, title).
type Target = (Text, Text)

isFigureTarget :: Target -> Maybe Target
isFigureTarget tgt
  | (src, Just tit) <- second (T.stripPrefix "fig:") tgt = Just (src, tit)
  | otherwise = Nothing

-- | Bidirectional patter synonym
--
-- It can pass as a Block constructor
--
-- >>> SimpleFigure nullAttr [] (T.pack "", T.pack "title")
-- Para [Image ("",[],[]) [] ("","fig:title")]
--
--
-- It can be used to pattern match
-- >>> let img = Para [Image undefined undefined (undefined, T.pack "title")]
-- >>> case img of { SimpleFigure _ _ _ -> True; _ -> False }
-- False
-- >>> let fig = Para [Image undefined undefined (undefined, T.pack "fig:title")]
-- >>> case fig of { SimpleFigure _ _ tit -> snd tit; _ -> T.pack "" }
-- "title"
pattern SimpleFigure :: Attr -> [inline] -> Target -> BlockF (InlineF block inline) block
pattern SimpleFigure attr figureCaption tgt <-
     Para [Image attr figureCaption
        (isFigureTarget -> Just tgt)]  where
  SimpleFigure attr figureCaption tgt =
    Para [Image attr figureCaption (second ("fig:" <>) tgt)]


-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Inline elements.
data InlineF block inline
    = Str Text            -- ^ Text (string)
    | Emph [inline]         -- ^ Emphasized text (list of inlines)
    | Underline [inline]    -- ^  Underlined text (list of inlines)
    | Strong [inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [inline] -- ^ Quoted text (list of inlines)
    | Cite [CitationF inline]  [inline] -- ^ Citation (list of inlines)
    | Code Attr Text      -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | Math MathType Text  -- ^ TeX math (literal)
    | RawInline Format Text -- ^ Raw inline
    | Link Attr [inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
    | Image Attr [inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [block]          -- ^ Footnote or endnote
    | Span Attr [inline]    -- ^ Generic inline container with attributes
    deriving ( Show, Eq, Ord, Read, Typeable, Data, Generic
             , Functor, Foldable, Traversable
             )

data CitationF inline = Citation
                         { citationId      :: Text
                         , citationPrefix  :: [inline]
                         , citationSuffix  :: [inline]
                         , citationMode    :: CitationMode
                         , citationNoteNum :: Int
                         , citationHash    :: Int
                         }
    deriving ( Show, Eq, Read, Typeable, Data, Generic
             , Functor, Foldable, Traversable
             )

instance Eq inline => Ord (CitationF inline) where
    compare = comparing citationHash

data CitationMode = AuthorInText | SuppressAuthor | NormalCitation
                    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)


-- ToJSON/FromJSON instances. Some are defined by hand so that we have
-- more control over the format.

$(let jsonOpts = defaultOptions
        { allNullaryToStringTag = False
        , sumEncoding = TaggedObject { tagFieldName = "t", contentsFieldName = "c" }
        }
  in fmap concat $ traverse (deriveJSON jsonOpts)
     [ ''MetaValueF
     , ''CitationMode
     , ''CitationF
     , ''QuoteType
     , ''MathType
     , ''ListNumberStyle
     , ''ListNumberDelim
     , ''Alignment
     , ''ColWidth
     , ''RowF
     , ''CaptionF
     , ''TableHeadF
     , ''TableBodyF
     , ''TableFootF
     , ''CellF
     , ''InlineF
     , ''BlockF
     ])

-- Instances for deepseq
instance (NFData block, NFData inline, NFData metaValue) => NFData (MetaValueF inline block metaValue)
instance NFData inline => NFData (CitationF inline)
instance NFData Alignment
instance NFData RowSpan
instance NFData ColSpan
instance NFData block => NFData (CellF block)
instance NFData block => NFData (RowF block)
instance NFData block => NFData (TableHeadF block)
instance NFData block => NFData (TableBodyF block)
instance NFData block => NFData (TableFootF block)
instance (NFData block, NFData inline) => NFData (CaptionF block inline)
instance (NFData block, NFData inline) => NFData (InlineF block inline)
instance NFData MathType
instance NFData Format
instance NFData CitationMode
instance NFData QuoteType
instance NFData ListNumberDelim
instance NFData ListNumberStyle
instance NFData ColWidth
instance NFData RowHeadColumns
instance (NFData inline, NFData block) => NFData (BlockF inline block)

pandocTypesVersion :: Version
pandocTypesVersion = version
