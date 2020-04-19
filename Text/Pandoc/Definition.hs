{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric,
    FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards, CPP #-}

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
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum)

-- | Alignment of a table column.
data Alignment = AlignLeft
               | AlignRight
               | AlignCenter
               | AlignDefault deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The width of a table column, as a fraction of the total table
-- width.
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

-- | A body of a table, with an intermediate head and the specified
-- number of row header columns.
data TableBody = TableBody Attr RowHeadColumns [Row] [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The foot of a table.
data TableFoot = TableFoot Attr [Row]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A short caption, for use in, for instance, lists of figures.
type ShortCaption = [Inline]

-- | The caption of a table, with an optional short caption.
data Caption = Caption (Maybe ShortCaption) [Block]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A table cell.
data Cell = Cell Attr Alignment RowSpan ColSpan [Block]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | The number of rows occupied by a cell; the height of a cell.
newtype RowSpan = RowSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum)

-- | The number of columns occupied by a cell; the width of a cell.
newtype ColSpan = ColSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum)

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
    -- | Table, with attributes, caption, optional short caption,
    -- column alignments and widths (required), table head, table
    -- bodies, and table foot
    | Table Attr Caption [ColSpec] TableHead [TableBody] TableFoot
    -- | Generic block container with attributes
    | Div Attr [Block]
    -- | Nothing
    | Null
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Link target (URL, title).
type Target = (Text, Text)

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


-- ToJSON/FromJSON instances. We do this by hand instead of deriving
-- from generics, so we can have more control over the format.

taggedNoContent :: Text -> Value
taggedNoContent x = object [ "t" .= x ]

tagged :: ToJSON a => Text -> a -> Value
tagged x y = object [ "t" .= x, "c" .= y ]

instance FromJSON MetaValue where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "MetaMap"     -> MetaMap     <$> (v .: "c")
      "MetaList"    -> MetaList    <$> (v .: "c")
      "MetaBool"    -> MetaBool    <$> (v .: "c")
      "MetaString"  -> MetaString  <$> (v .: "c")
      "MetaInlines" -> MetaInlines <$> (v .: "c")
      "MetaBlocks"  -> MetaBlocks  <$> (v .: "c")
      _ -> mempty
  parseJSON _ = mempty
instance ToJSON MetaValue where
  toJSON (MetaMap mp) = tagged "MetaMap" mp
  toJSON (MetaList lst) = tagged "MetaList" lst
  toJSON (MetaBool bool) = tagged "MetaBool" bool
  toJSON (MetaString s) = tagged "MetaString" s
  toJSON (MetaInlines ils) = tagged "MetaInlines" ils
  toJSON (MetaBlocks blks) = tagged "MetaBlocks" blks

instance FromJSON Meta where
  parseJSON j = Meta <$> parseJSON j
instance ToJSON Meta where
  toJSON meta = toJSON $ unMeta meta

instance FromJSON CitationMode where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "AuthorInText"   -> return AuthorInText
      "SuppressAuthor" -> return SuppressAuthor
      "NormalCitation" -> return NormalCitation
      _ -> mempty
  parseJSON _ = mempty
instance ToJSON CitationMode where
  toJSON cmode = taggedNoContent s
    where s = case cmode of
            AuthorInText   -> "AuthorInText"
            SuppressAuthor -> "SuppressAuthor"
            NormalCitation -> "NormalCitation"


instance FromJSON Citation where
  parseJSON (Object v) = do
    citationId'      <- v .: "citationId"
    citationPrefix'  <- v .: "citationPrefix"
    citationSuffix'  <- v .: "citationSuffix"
    citationMode'    <- v .: "citationMode"
    citationNoteNum' <- v .: "citationNoteNum"
    citationHash'    <- v .: "citationHash"
    return Citation { citationId = citationId'
                    , citationPrefix = citationPrefix'
                    , citationSuffix = citationSuffix'
                    , citationMode = citationMode'
                    , citationNoteNum = citationNoteNum'
                    , citationHash = citationHash'
                    }
  parseJSON _ = mempty
instance ToJSON Citation where
  toJSON cit =
    object [ "citationId"      .= citationId cit
           , "citationPrefix"  .= citationPrefix cit
           , "citationSuffix"  .= citationSuffix cit
           , "citationMode"    .= citationMode cit
           , "citationNoteNum" .= citationNoteNum cit
           , "citationHash"    .= citationHash cit
           ]

instance FromJSON QuoteType where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "SingleQuote" -> return SingleQuote
      "DoubleQuote" -> return DoubleQuote
      _                    -> mempty
  parseJSON _ = mempty
instance ToJSON QuoteType where
  toJSON qtype = taggedNoContent s
    where s = case qtype of
            SingleQuote -> "SingleQuote"
            DoubleQuote -> "DoubleQuote"


instance FromJSON MathType where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "DisplayMath" -> return DisplayMath
      "InlineMath"  -> return InlineMath
      _                    -> mempty
  parseJSON _ = mempty
instance ToJSON MathType where
  toJSON mtype = taggedNoContent s
    where s = case mtype of
            DisplayMath -> "DisplayMath"
            InlineMath  -> "InlineMath"

instance FromJSON ListNumberStyle where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "DefaultStyle" -> return DefaultStyle
      "Example"      -> return Example
      "Decimal"      -> return Decimal
      "LowerRoman"   -> return LowerRoman
      "UpperRoman"   -> return UpperRoman
      "LowerAlpha"   -> return LowerAlpha
      "UpperAlpha"   -> return UpperAlpha
      _              -> mempty
  parseJSON _ = mempty
instance ToJSON ListNumberStyle where
  toJSON lsty = taggedNoContent s
    where s = case lsty of
            DefaultStyle -> "DefaultStyle"
            Example      -> "Example"
            Decimal      -> "Decimal"
            LowerRoman   -> "LowerRoman"
            UpperRoman   -> "UpperRoman"
            LowerAlpha   -> "LowerAlpha"
            UpperAlpha   -> "UpperAlpha"

instance FromJSON ListNumberDelim where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "DefaultDelim" -> return DefaultDelim
      "Period"       -> return Period
      "OneParen"     -> return OneParen
      "TwoParens"    -> return TwoParens
      _                     -> mempty
  parseJSON _ = mempty
instance ToJSON ListNumberDelim where
  toJSON delim = taggedNoContent s
    where s = case delim of
            DefaultDelim -> "DefaultDelim"
            Period       -> "Period"
            OneParen     -> "OneParen"
            TwoParens    -> "TwoParens"

instance FromJSON Alignment where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "AlignLeft"    -> return AlignLeft
      "AlignRight"   -> return AlignRight
      "AlignCenter"  -> return AlignCenter
      "AlignDefault" -> return AlignDefault
      _                     -> mempty
  parseJSON _ = mempty
instance ToJSON Alignment where
  toJSON delim = taggedNoContent s
    where s = case delim of
            AlignLeft    -> "AlignLeft"
            AlignRight   -> "AlignRight"
            AlignCenter  -> "AlignCenter"
            AlignDefault -> "AlignDefault"

instance FromJSON ColWidth where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "ColWidth"        -> ColWidth <$> v .: "c"
      "ColWidthDefault" -> return ColWidthDefault
      _     -> mempty
  parseJSON _ = mempty
instance ToJSON ColWidth where
  toJSON (ColWidth ils)  = tagged "ColWidth" ils
  toJSON ColWidthDefault = taggedNoContent "ColWidthDefault"

instance FromJSON Row where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "Row" -> do (attr, body) <- v .: "c"
                  return $ Row attr body
      _     -> mempty
  parseJSON _ = mempty
instance ToJSON Row where
  toJSON (Row attr body) = tagged "Row" (attr, body)

instance FromJSON Caption where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "Caption" -> do (mshort, body) <- v .: "c"
                      return $ Caption mshort body
      _     -> mempty
  parseJSON _ = mempty
instance ToJSON Caption where
  toJSON (Caption mshort body) = tagged "Caption" (mshort, body)

instance FromJSON RowSpan where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "RowSpan" -> RowSpan <$> v .: "c"
      _         -> mempty
  parseJSON _ = mempty
instance ToJSON RowSpan where
  toJSON (RowSpan h)  = tagged "RowSpan" h

instance FromJSON ColSpan where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "ColSpan" -> ColSpan <$> v .: "c"
      _         -> mempty
  parseJSON _ = mempty
instance ToJSON ColSpan where
  toJSON (ColSpan w)  = tagged "ColSpan" w

instance FromJSON RowHeadColumns where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "RowHeadColumns" -> RowHeadColumns <$> v .: "c"
      _                -> mempty
  parseJSON _ = mempty
instance ToJSON RowHeadColumns where
  toJSON (RowHeadColumns w)  = tagged "RowHeadColumns" w

instance FromJSON TableHead where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "TableHead" -> do (attr, body) <- v .: "c"
                        return $ TableHead attr body
      _           -> mempty
  parseJSON _ = mempty
instance ToJSON TableHead where
  toJSON (TableHead attr body) = tagged "TableHead" (attr, body)

instance FromJSON TableBody where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "TableBody" -> do (attr, rhc, hd, body) <- v .: "c"
                        return $ TableBody attr rhc hd body
      _           -> mempty
  parseJSON _ = mempty
instance ToJSON TableBody where
  toJSON (TableBody attr rhc hd body) = tagged "TableBody" (attr, rhc, hd, body)

instance FromJSON TableFoot where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "TableFoot" -> do (attr, body) <- v .: "c"
                        return $ TableFoot attr body
      _           -> mempty
  parseJSON _ = mempty
instance ToJSON TableFoot where
  toJSON (TableFoot attr body) = tagged "TableFoot" (attr, body)

instance FromJSON Cell where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "Cell" -> do (attr, malign, rs, cs, body) <- v .: "c"
                   return $ Cell attr malign rs cs body
      _     -> mempty
  parseJSON _ = mempty
instance ToJSON Cell where
  toJSON (Cell attr malign rs cs body) = tagged "Cell" (attr, malign, rs, cs, body)

instance FromJSON Inline where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "Str"         -> Str <$> v .: "c"
      "Emph"        -> Emph <$> v .: "c"
      "Underline"   -> Underline <$> v .: "c"
      "Strong"      -> Strong <$> v .: "c"
      "Strikeout"   -> Strikeout <$> v .: "c"
      "Superscript" -> Superscript <$> v .: "c"
      "Subscript"   -> Subscript <$> v .: "c"
      "SmallCaps"   -> SmallCaps <$> v .: "c"
      "Quoted"      -> do (qt, ils) <- v .: "c"
                          return $ Quoted qt ils
      "Cite"        -> do (cits, ils) <- v .: "c"
                          return $ Cite cits ils
      "Code"        -> do (attr, s) <- v .: "c"
                          return $ Code attr s
      "Space"       -> return Space
      "SoftBreak"   -> return SoftBreak
      "LineBreak"   -> return LineBreak
      "Math"        -> do (mtype, s) <- v .: "c"
                          return $ Math mtype s
      "RawInline"   -> do (fmt, s) <- v .: "c"
                          return $ RawInline fmt s
      "Link"        -> do (attr, ils, tgt) <- v .: "c"
                          return $ Link attr ils tgt
      "Image"       -> do (attr, ils, tgt) <- v .: "c"
                          return $ Image attr ils tgt
      "Note"        -> Note <$> v .: "c"
      "Span"        -> do (attr, ils) <- v .: "c"
                          return $ Span attr ils
      _ -> mempty
  parseJSON _ = mempty

instance ToJSON Inline where
  toJSON (Str s) = tagged "Str" s
  toJSON (Emph ils) = tagged "Emph" ils
  toJSON (Underline ils) = tagged "Underline" ils
  toJSON (Strong ils) = tagged "Strong" ils
  toJSON (Strikeout ils) = tagged "Strikeout" ils
  toJSON (Superscript ils) = tagged "Superscript" ils
  toJSON (Subscript ils) = tagged "Subscript" ils
  toJSON (SmallCaps ils) = tagged "SmallCaps" ils
  toJSON (Quoted qtype ils) = tagged "Quoted" (qtype, ils)
  toJSON (Cite cits ils) = tagged "Cite" (cits, ils)
  toJSON (Code attr s) = tagged "Code" (attr, s)
  toJSON Space = taggedNoContent "Space"
  toJSON SoftBreak = taggedNoContent "SoftBreak"
  toJSON LineBreak = taggedNoContent "LineBreak"
  toJSON (Math mtype s) = tagged "Math" (mtype, s)
  toJSON (RawInline fmt s) = tagged "RawInline" (fmt, s)
  toJSON (Link attr ils target) = tagged "Link" (attr, ils, target)
  toJSON (Image attr ils target) = tagged "Image" (attr, ils, target)
  toJSON (Note blks) = tagged "Note" blks
  toJSON (Span attr ils) = tagged "Span" (attr, ils)

instance FromJSON Block where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "Plain"          -> Plain <$> v .: "c"
      "Para"           -> Para  <$> v .: "c"
      "LineBlock"      -> LineBlock <$> v .: "c"
      "CodeBlock"      -> do (attr, s) <- v .: "c"
                             return $ CodeBlock attr s
      "RawBlock"       -> do (fmt, s) <- v .: "c"
                             return $ RawBlock fmt s
      "BlockQuote"     -> BlockQuote <$> v .: "c"
      "OrderedList"    -> do (attr, items) <- v .: "c"
                             return $ OrderedList attr items
      "BulletList"     -> BulletList <$> v .: "c"
      "DefinitionList" -> DefinitionList <$> v .: "c"
      "Header"         -> do (n, attr, ils) <- v .: "c"
                             return $ Header n attr ils
      "HorizontalRule" -> return HorizontalRule
      "Table"          -> do (attr, cpt, align, hdr, body, foot) <- v .: "c"
                             return $ Table attr cpt align hdr body foot
      "Div"            -> do (attr, blks) <- v .: "c"
                             return $ Div attr blks
      "Null"           -> return Null
      _                -> mempty
  parseJSON _ = mempty
instance ToJSON Block where
  toJSON (Plain ils) = tagged "Plain" ils
  toJSON (Para ils) = tagged "Para" ils
  toJSON (LineBlock lns) = tagged "LineBlock" lns
  toJSON (CodeBlock attr s) = tagged "CodeBlock" (attr, s)
  toJSON (RawBlock fmt s) = tagged "RawBlock" (fmt, s)
  toJSON (BlockQuote blks) = tagged "BlockQuote" blks
  toJSON (OrderedList listAttrs blksList) = tagged "OrderedList" (listAttrs, blksList)
  toJSON (BulletList blksList) = tagged "BulletList" blksList
  toJSON (DefinitionList defs) = tagged "DefinitionList" defs
  toJSON (Header n attr ils) = tagged "Header" (n, attr, ils)
  toJSON HorizontalRule = taggedNoContent "HorizontalRule"
  toJSON (Table attr caption aligns hd body foot) =
    tagged "Table" (attr, caption, aligns, hd, body, foot)
  toJSON (Div attr blks) = tagged "Div" (attr, blks)
  toJSON Null = taggedNoContent "Null"

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
