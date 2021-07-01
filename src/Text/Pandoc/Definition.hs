{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric,
    FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards, CPP,
    TemplateHaskell , PatternSynonyms, ViewPatterns #-}

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
module Text.Pandoc.Definition ( Pandoc(..)
                              , Meta(..)
                              , MetaValue(..)
                              , nullMeta
                              , isNullMeta
                              , lookupMeta
                              , docTitle
                              , docAuthors
                              , docDate
                              , Block(..)
                              , pattern SimpleFigure
                              , Inline(..)
                              , ListAttributes
                              , ListNumberStyle(..)
                              , ListNumberDelim(..)
                              , Format(..)
                              , Attr
                              , nullAttr
                              , Caption(..)
                              , ShortCaption
                              , RowHeadColumns(..)
                              , Alignment(..)
                              , ColWidth(..)
                              , ColSpec
                              , Row(..)
                              , TableHead(..)
                              , TableBody(..)
                              , TableFoot(..)
                              , Cell(..)
                              , RowSpan(..)
                              , ColSpan(..)
                              , QuoteType(..)
                              , Target
                              , MathType(..)
                              , Citation(..)
                              , CitationMode(..)
                              , pandocTypesVersion
                              ) where

import Data.Generics (Data, Typeable)
import Data.Ord (comparing)
import Data.Aeson hiding (Null)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.String
import Control.DeepSeq
import Paths_pandoc_types (version)
import Data.Version (Version, versionBranch)
import Data.Semigroup (Semigroup(..))
import Control.Arrow (second)

data Pandoc = Pandoc Meta [Block]
              deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance Semigroup Pandoc where
  (Pandoc m1 bs1) <> (Pandoc m2 bs2) =
    Pandoc (m1 <> m2) (bs1 <> bs2)
instance Monoid Pandoc where
  mempty = Pandoc mempty mempty
  mappend = (<>)

-- | Metadata for the document:  title, authors, date.
newtype Meta = Meta { unMeta :: M.Map Text MetaValue }
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance Semigroup Meta where
  (Meta m1) <> (Meta m2) = Meta (M.union m2 m1)
  -- note: M.union is left-biased, so if there are fields in both m2
  -- and m1, m2 wins.
instance Monoid Meta where
  mempty = Meta M.empty
  mappend = (<>)

data MetaValue = MetaMap (M.Map Text MetaValue)
               | MetaList [MetaValue]
               | MetaBool Bool
               | MetaString Text
               | MetaInlines [Inline]
               | MetaBlocks [Block]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

nullMeta :: Meta
nullMeta = Meta M.empty

isNullMeta :: Meta -> Bool
isNullMeta (Meta m) = M.null m

-- Helper functions to extract metadata

-- | Retrieve the metadata value for a given @key@.
lookupMeta :: Text -> Meta -> Maybe MetaValue
lookupMeta key (Meta m) = M.lookup key m

-- | Extract document title from metadata; works just like the old @docTitle@.
docTitle :: Meta -> [Inline]
docTitle meta =
  case lookupMeta "title" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | Extract document authors from metadata; works just like the old
-- @docAuthors@.
docAuthors :: Meta -> [[Inline]]
docAuthors meta =
  case lookupMeta "author" meta of
        Just (MetaString s)    -> [[Str s]]
        Just (MetaInlines ils) -> [ils]
        Just (MetaList   ms)   -> [ils | MetaInlines ils <- ms] ++
                                  [ils | MetaBlocks [Plain ils] <- ms] ++
                                  [ils | MetaBlocks [Para ils]  <- ms] ++
                                  [[Str x] | MetaString x <- ms]
        _                      -> []

-- | Extract date from metadata; works just like the old @docDate@.
docDate :: Meta -> [Inline]
docDate meta =
  case lookupMeta "date" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

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
data Row = Row Attr [Cell]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The head of a table.
data TableHead = TableHead Attr [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A body of a table, with an intermediate head, intermediate body,
-- and the specified number of row header columns in the intermediate
-- body.
data TableBody = TableBody Attr RowHeadColumns [Row] [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The foot of a table.
data TableFoot = TableFoot Attr [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A short caption, for use in, for instance, lists of figures.
type ShortCaption = [Inline]

-- | The caption of a figure, with optional short caption.
data Caption = Caption Attr (Maybe ShortCaption) [Block]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A table cell.
data Cell = Cell Attr Alignment RowSpan ColSpan [Block]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The number of rows occupied by a cell; the height of a cell.
newtype RowSpan = RowSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | The number of columns occupied by a cell; the width of a cell.
newtype ColSpan = ColSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | Block element.
data Block
    -- | Plain text, not a paragraph
    = Plain [Inline]
    -- | Paragraph
    | Para [Inline]
    -- | Multiple non-breaking lines
    | LineBlock [[Inline]]
    -- | Code block (literal) with attributes
    | CodeBlock Attr Text
    -- | Raw block
    | RawBlock Format Text
    -- | Block quote (list of blocks)
    | BlockQuote [Block]
    -- | Ordered list (attributes and a list of items, each a list of
    -- blocks)
    | OrderedList ListAttributes [[Block]]
    -- | Bullet list (list of items, each a list of blocks)
    | BulletList [[Block]]
    -- | Definition list. Each list item is a pair consisting of a
    -- term (a list of inlines) and one or more definitions (each a
    -- list of blocks)
    | DefinitionList [([Inline],[[Block]])]
    -- | Header - level (integer) and text (inlines)
    | Header Int Attr [Inline]
    -- | Horizontal rule
    | HorizontalRule
    -- | Table, with attributes, column alignments and widths
    -- (required), table head, table bodies, and table foot
    | Table Attr Caption [ColSpec] TableHead [TableBody] TableFoot
    -- | Figure, with attributes, caption and caption position, width
    -- (optional), and content (list of blocks)
    | Figure Attr Caption [Block]
    -- | Generic block container with attributes
    | Div Attr [Block]
    -- | Nothing
    | Null
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

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
pattern SimpleFigure :: Attr -> [Inline] -> Target -> Block
pattern SimpleFigure attr figureCaption tgt <-
    Para [Image attr figureCaption
        (isFigureTarget -> Just tgt)]  where
  SimpleFigure attr figureCaption tgt =
    Para [Image attr figureCaption (second ("fig:" <>) tgt)]


-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Inline elements.
data Inline
    = Str Text            -- ^ Text (string)
    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
    | Underline [Inline]    -- ^  Underlined text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
    | Code Attr Text      -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | Math MathType Text  -- ^ TeX math (literal)
    | RawInline Format Text -- ^ Raw inline
    | Link Attr [Inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
    | Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [Block]          -- ^ Footnote or endnote
    | Span Attr [Inline]    -- ^ Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data Citation = Citation { citationId      :: Text
                         , citationPrefix  :: [Inline]
                         , citationSuffix  :: [Inline]
                         , citationMode    :: CitationMode
                         , citationNoteNum :: Int
                         , citationHash    :: Int
                         }
                deriving (Show, Eq, Read, Typeable, Data, Generic)

instance Ord Citation where
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
     [ ''MetaValue
     , ''CitationMode
     , ''Citation
     , ''QuoteType
     , ''MathType
     , ''ListNumberStyle
     , ''ListNumberDelim
     , ''Alignment
     , ''ColWidth
     , ''Row
     , ''Caption
     , ''TableHead
     , ''TableBody
     , ''TableFoot
     , ''Cell
     , ''Inline
     , ''Block
     ])

instance FromJSON Meta where
  parseJSON = fmap Meta . parseJSON
instance ToJSON Meta where
  toJSON (Meta m) = toJSON m
  toEncoding (Meta m) = toEncoding m

instance FromJSON Pandoc where
  parseJSON (Object v) = do
    mbJVersion <- v .:? "pandoc-api-version" :: Aeson.Parser (Maybe [Int])
    case mbJVersion of
      Just jVersion  | x : y : _ <- jVersion
                     , x' : y' : _ <- versionBranch pandocTypesVersion
                     , x == x'
                     , y == y' -> Pandoc <$> v .: "meta" <*> v .: "blocks"
                     | otherwise ->
                         fail $ mconcat [ "Incompatible API versions: "
                                        , "encoded with "
                                        , show jVersion
                                        , " but attempted to decode with "
                                        , show $ versionBranch pandocTypesVersion
                                        , "."
                                        ]
      _ -> fail "JSON missing pandoc-api-version."
  parseJSON _ = mempty
instance ToJSON Pandoc where
  toJSON (Pandoc meta blks) =
    object [ "pandoc-api-version" .= versionBranch pandocTypesVersion
           , "meta"               .= meta
           , "blocks"             .= blks
           ]
  toEncoding (Pandoc meta blks) =
    pairs $ mconcat [ "pandoc-api-version" .= versionBranch pandocTypesVersion
                    , "meta"               .= meta
                    , "blocks"             .= blks
                    ]

-- Instances for deepseq
instance NFData MetaValue
instance NFData Meta
instance NFData Citation
instance NFData Alignment
instance NFData RowSpan
instance NFData ColSpan
instance NFData Cell
instance NFData Row
instance NFData TableHead
instance NFData TableBody
instance NFData TableFoot
instance NFData Caption
instance NFData Inline
instance NFData MathType
instance NFData Format
instance NFData CitationMode
instance NFData QuoteType
instance NFData ListNumberDelim
instance NFData ListNumberStyle
instance NFData ColWidth
instance NFData RowHeadColumns
instance NFData Block
instance NFData Pandoc

pandocTypesVersion :: Version
pandocTypesVersion = version
