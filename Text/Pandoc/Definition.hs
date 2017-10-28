{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric,
FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards, CPP,
UndecidableInstances, ScopedTypeVariables, FlexibleInstances #-}

{-
Copyright (c) 2006-2016, John MacFarlane

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
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definition of 'Pandoc' data structure for format-neutral representation
of documents.
-}
module Text.Pandoc.Definition ( Pandoc
                              , Pandoc'(..)
                              , Meta
                              , Meta'(..)
                              , MetaValue
                              , MetaValue'(..)
                              , nullMeta
                              , isNullMeta
                              , lookupMeta
                              , docTitle
                              , docAuthors
                              , docDate
                              , Block
                              , Block'(..)
                              , Inline
                              , Inline'(..)
                              , Alignment(..)
                              , ListAttributes
                              , ListNumberStyle(..)
                              , ListNumberDelim(..)
                              , Format(..)
                              , Attr
                              , Attr'(..)
                              , nullAttr
                              , TableCell
                              , TableCell'
                              , QuoteType(..)
                              , Target
                              , Target'
                              , MathType(..)
                              , Citation
                              , Citation'(..)
                              , CitationMode(..)
                              , pandocTypesVersion
                              ) where

import Data.String.Conversions
import Data.Generics (Data, Typeable)
import Data.Ord (comparing)
import Data.Aeson hiding (Null)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import GHC.Generics (Generic)
import Data.String
import Data.Char (toLower)
#if MIN_VERSION_base(4,8,0)
import Control.DeepSeq
#else
import Data.Monoid
import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq.Generics
#endif
import Paths_pandoc_types (version)
import Data.Version (Version, versionBranch)

type Pandoc = Pandoc' String

data Pandoc' string = Pandoc (Meta' string) [Block' string]
              deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance (Ord string, Monoid string) => Monoid (Pandoc' string) where
  mempty = Pandoc mempty mempty
  (Pandoc m1 bs1) `mappend` (Pandoc m2 bs2) =
    Pandoc (m1 `mappend` m2) (bs1 `mappend` bs2)

type Meta = Meta' String

-- | Metadata for the document:  title, authors, date.
newtype Meta' string = Meta { unMeta :: M.Map string (MetaValue' string) }
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance (Ord string, Monoid string) => Monoid (Meta' string) where
  mempty = Meta mempty
  (Meta m1) `mappend` (Meta m2) = Meta (M.union m1 m2)
  -- note: M.union is left-biased, so if there are fields in both m1
  -- and m2, m1 wins.

type MetaValue = MetaValue' String

data MetaValue' string
               = MetaMap (M.Map string (MetaValue' string))
               | MetaList [MetaValue' string]
               | MetaBool Bool
               | MetaString string
               | MetaInlines [Inline' string]
               | MetaBlocks [Block' string]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

nullMeta :: Ord string => Meta' string
nullMeta = Meta mempty

isNullMeta :: Meta' string -> Bool
isNullMeta (Meta m) = M.null m

-- Helper functions to extract metadata

-- | Retrieve the metadata value for a given @key@.
lookupMeta :: (Ord string) => string -> Meta' string -> Maybe (MetaValue' string)
lookupMeta key (Meta m) = M.lookup key m

-- | Extract document title from metadata; works just like the old @docTitle@.
docTitle :: (IsString string, Ord string) => Meta' string -> [Inline' string]
docTitle meta =
  case lookupMeta "title" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | Extract document authors from metadata; works just like the old
-- @docAuthors@.
docAuthors :: (Ord string, IsString string) => Meta' string -> [[Inline' string]]
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
docDate :: (Ord string, IsString string) => Meta' string -> [Inline' string]
docDate meta =
  case lookupMeta "date" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | Alignment of a table column.
data Alignment = AlignLeft
               | AlignRight
               | AlignCenter
               | AlignDefault deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | List attributes.
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

type Attr = Attr' String

-- | Attributes: identifier, classes, key-value pairs
newtype Attr' string = Attr (string, [string], [(string, string)])
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance FromJSON string => FromJSON (Attr' string) where
  parseJSON v = Attr <$> parseJSON v

instance ToJSON string => ToJSON (Attr' string) where
  toJSON (Attr d) = toJSON d

nullAttr :: IsString string => Attr' string
nullAttr = Attr ("",[],[])

type TableCell = TableCell' String

-- | Table cells are list of Blocks
type TableCell' string = [Block' string]

-- | Formats for raw blocks
--
-- TODO: changing this type to use ST or LT internally is a breaking change, and
-- using a polymorphic type like with the others is a bit tricky, since we need
-- to suddenly write lots of instances manually that could formerly be derived.
-- try it and follow the type errors!
--
-- TODO: introduce @mkFormat = Format . map toLower . cs :: ConvertibleStrings
-- string String => string -> Format@ and make 'Format' abstract (do not export
-- constructor), then we don't have to worry about Eq, Ord distinguishing upper
-- and lower case.
newtype Format = Format String
               deriving (Read, Show, Typeable, Data, Generic, ToJSON, FromJSON)

instance IsString Format where
  fromString f = Format $ map toLower f

instance Eq Format where
  Format x == Format y = map toLower x == map toLower y

instance Ord Format where
  compare (Format x) (Format y) = compare (map toLower x) (map toLower y)

type Block = Block' String

-- | Block element.
data Block' string
    = Plain [Inline' string] -- ^ Plain text, not a paragraph
    | Para [Inline' string]  -- ^ Paragraph
    | LineBlock [[Inline' string]]  -- ^ Multiple non-breaking lines
    | CodeBlock (Attr' string) string -- ^ Code block (literal) with attributes
    | RawBlock Format string -- ^ Raw block
    | BlockQuote [Block' string]    -- ^ Block quote (list of blocks)
    | OrderedList ListAttributes [[Block' string]] -- ^ Ordered list (attributes
                            -- and a list of items, each a list of blocks)
    | BulletList [[Block' string]]  -- ^ Bullet list (list of items, each
                            -- a list of blocks)
    | DefinitionList [([Inline' string],[[Block' string]])]  -- ^ Definition list
                            -- Each list item is a pair consisting of a
                            -- term (a list of inlines) and one or more
                            -- definitions (each a list of blocks)
    | Header Int (Attr' string) [Inline' string] -- ^ Header - level (integer) and text (inlines)
    | HorizontalRule        -- ^ Horizontal rule
    | Table [Inline' string] [Alignment] [Double] [TableCell' string] [[TableCell' string]]  -- ^ Table,
                            -- with caption, column alignments (required),
                            -- relative column widths (0 = default),
                            -- column headers (each a list of blocks), and
                            -- rows (each a list of lists of blocks)
    | Div (Attr' string) [Block' string]      -- ^ Generic block container with attributes
    | Null                  -- ^ Nothing
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

type Target = Target' String

-- | Link target (URL, title).
type Target' string = (string, string)

-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

type Inline = Inline' String

-- | Inline elements.
data Inline' string
    = Str string                 -- ^ Text
    | Emph [Inline' string]         -- ^ Emphasized text (list of inlines)
    | Strong [Inline' string]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline' string]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline' string]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline' string]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline' string]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline' string] -- ^ Quoted text (list of inlines)
    | Cite [Citation' string] [Inline' string] -- ^ Citation (list of inlines)
    | Code (Attr' string) string      -- ^ Inline' string code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | Math MathType string  -- ^ TeX math (literal)
    | RawInline Format string -- ^ Raw inline
    | Link (Attr' string) [Inline' string] (Target' string)
           -- ^ Hyperlink: alt text (list of inlines), target
    | Image (Attr' string) [Inline' string] (Target' string)
           -- ^ Image:  alt text (list of inlines), target
    | Note [Block' string]          -- ^ Footnote or endnote
    | Span (Attr' string) [Inline' string]    -- ^ Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

type Citation = Citation' String

data Citation' string =
                Citation { citationId      :: string
                         , citationPrefix  :: [Inline' string]
                         , citationSuffix  :: [Inline' string]
                         , citationMode    :: CitationMode
                         , citationNoteNum :: Int
                         , citationHash    :: Int
                         }
                deriving (Show, Eq, Read, Typeable, Data, Generic)

instance Ord string => Ord (Citation' string) where
    compare = comparing citationHash

data CitationMode = AuthorInText | SuppressAuthor | NormalCitation
                    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)


-- ToJSON/FromJSON instances. We do this by hand instead of deriving
-- from generics, so we can have more control over the format.

taggedNoContent :: [Char] -> Value
taggedNoContent x = object [ "t" .= x ]

tagged :: ToJSON a => [Char] -> a -> Value
tagged x y = object [ "t" .= x, "c" .= y ]

instance (Ord string, FromJSONKey string, FromJSON string, ConvertibleStrings ST string)
       => FromJSON (MetaValue' string) where
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
instance (Ord string, ToJSONKey string, ToJSON string, ConvertibleStrings string ST)
       => ToJSON (MetaValue' string) where
  toJSON (MetaMap mp) = tagged "MetaMap" mp
  toJSON (MetaList lst) = tagged "MetaList" lst
  toJSON (MetaBool bool) = tagged "MetaBool" bool
  toJSON (MetaString s) = tagged "MetaString" s
  toJSON (MetaInlines ils) = tagged "MetaInlines" ils
  toJSON (MetaBlocks blks) = tagged "MetaBlocks" blks

instance ( Ord string, FromJSONKey string, FromJSON string
         , ConvertibleStrings ST string, ConvertibleStrings string ST)
            => FromJSON (Meta' string) where
  parseJSON j = Meta <$> parseJSON j
instance (Ord string, ToJSONKey string, ToJSON string, ConvertibleStrings string ST)
            => ToJSON (Meta' string) where
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


instance (ConvertibleStrings ST string, FromJSON string)
             => FromJSON (Citation' string) where
  parseJSON (Object v) = do
    citationId'      <- (cs :: ST -> string) <$> (v .: "citationId")
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
instance (ToJSON string, ConvertibleStrings string ST) => ToJSON (Citation' string) where
  toJSON cit =
    object [ "citationId"      .= (cs (citationId cit) :: ST)
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


instance (ConvertibleStrings ST string, FromJSON string)
          => FromJSON (Inline' string) where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value

    case t of
      "Str"         -> Str <$> v .: "c"
      "Emph"        -> Emph <$> v .: "c"
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

instance (ConvertibleStrings string ST, ToJSON string) => ToJSON (Inline' string) where
  toJSON (Str s) = tagged "Str" s
  toJSON (Emph ils) = tagged "Emph" ils
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

instance (FromJSON string, ConvertibleStrings ST string)
         => FromJSON (Block' string) where
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
      "HorizontalRule" -> return $ HorizontalRule
      "Table"          -> do (cpt, align, wdths, hdr, rows) <- v .: "c"
                             return $ Table cpt align wdths hdr rows
      "Div"            -> do (attr, blks) <- v .: "c"
                             return $ Div attr blks
      "Null"           -> return $ Null
      _                -> mempty
  parseJSON _ = mempty
instance (ConvertibleStrings string ST, ToJSON string)
          => ToJSON (Block' string) where
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
  toJSON (Table caption aligns widths cells rows) =
    tagged "Table" (caption, aligns, widths, cells, rows)
  toJSON (Div attr blks) = tagged "Div" (attr, blks)
  toJSON Null = taggedNoContent "Null"

instance ( Ord string, FromJSONKey string, FromJSON string
         , ConvertibleStrings ST string, ConvertibleStrings string ST)
           => FromJSON (Pandoc' string) where
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
instance (Ord string, ToJSONKey string, ToJSON string, ConvertibleStrings string ST)
            => ToJSON (Pandoc' string) where
  toJSON (Pandoc meta blks) =
    object [ "pandoc-api-version" .= versionBranch pandocTypesVersion
           , "meta"               .= meta
           , "blocks"             .= blks
           ]

-- Instances for deepseq
#if MIN_VERSION_base(4,8,0)
instance NFData string => NFData (MetaValue' string)
instance NFData string => NFData (Meta' string)
instance NFData string => NFData (Citation' string)
instance NFData Alignment
instance NFData string => NFData (Inline' string)
instance NFData string => NFData (Attr' string)
instance NFData MathType
instance NFData Format
instance NFData CitationMode
instance NFData QuoteType
instance NFData ListNumberDelim
instance NFData ListNumberStyle
instance NFData string => NFData (Block' string)
instance NFData string => NFData (Pandoc' string)
#else
instance NFData MetaValue where rnf = genericRnf
instance NFData Meta where rnf = genericRnf
instance NFData Citation where rnf = genericRnf
instance NFData Alignment where rnf = genericRnf
instance NFData Inline where rnf = genericRnf
instance NFData MathType where rnf = genericRnf
instance NFData Format where rnf = genericRnf
instance NFData CitationMode where rnf = genericRnf
instance NFData QuoteType where rnf = genericRnf
instance NFData ListNumberDelim where rnf = genericRnf
instance NFData ListNumberStyle where rnf = genericRnf
instance NFData Block where rnf = genericRnf
instance NFData Pandoc where rnf = genericRnf
#endif

pandocTypesVersion :: Version
pandocTypesVersion = version
