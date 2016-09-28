{-# LANGUAGE OverloadedStrings, OverloadedLists, DeriveDataTypeable,
             DeriveGeneric, FlexibleContexts, GeneralizedNewtypeDeriving,
             CPP
#-}

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
                              , Alignment(..)
                              , ListAttributes
                              , ListNumberStyle(..)
                              , ListNumberDelim(..)
                              , Format(..)
                              , Attr
                              , nullAttr
                              , TableCell
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
import Control.Monad (guard)
import qualified Data.Map as M
import GHC.Generics (Generic, Rep (..))
import Data.String
import Data.Char (toLower)
import Data.Monoid
#if MIN_VERSION_base(4,8,0)
import Control.DeepSeq
#else
import Control.DeepSeq.Generics
#endif
import Paths_pandoc_types (version)
import Data.Version (Version)

data Pandoc = Pandoc Meta [Block]
              deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance Monoid Pandoc where
  mempty = Pandoc mempty mempty
  (Pandoc m1 bs1) `mappend` (Pandoc m2 bs2) =
    Pandoc (m1 `mappend` m2) (bs1 `mappend` bs2)

-- | Metadata for the document:  title, authors, date.
newtype Meta = Meta { unMeta :: M.Map String MetaValue }
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance Monoid Meta where
  mempty = Meta (M.empty)
  (Meta m1) `mappend` (Meta m2) = Meta (M.union m1 m2)
  -- note: M.union is left-biased, so if there are fields in both m1
  -- and m2, m1 wins.

data MetaValue = MetaMap (M.Map String MetaValue)
               | MetaList [MetaValue]
               | MetaBool Bool
               | MetaString String
               | MetaInlines [Inline]
               | MetaBlocks [Block]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

nullMeta :: Meta
nullMeta = Meta M.empty

isNullMeta :: Meta -> Bool
isNullMeta (Meta m) = M.null m

-- Helper functions to extract metadata

-- | Retrieve the metadata value for a given @key@.
lookupMeta :: String -> Meta -> Maybe MetaValue
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

-- | Attributes: identifier, classes, key-value pairs
type Attr = (String, [String], [(String, String)])

nullAttr :: Attr
nullAttr = ("",[],[])

-- | Table cells are list of Blocks
type TableCell = [Block]

-- | Formats for raw blocks
newtype Format = Format String
               deriving (Read, Show, Typeable, Data, Generic, ToJSON, FromJSON)

instance IsString Format where
  fromString f = Format $ map toLower f

instance Eq Format where
  Format x == Format y = map toLower x == map toLower y

instance Ord Format where
  compare (Format x) (Format y) = compare (map toLower x) (map toLower y)

-- | Block element.
data Block
    = Plain [Inline]        -- ^ Plain text, not a paragraph
    | Para [Inline]         -- ^ Paragraph
    | CodeBlock Attr String -- ^ Code block (literal) with attributes
    | RawBlock Format String -- ^ Raw block
    | BlockQuote [Block]    -- ^ Block quote (list of blocks)
    | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
                            -- and a list of items, each a list of blocks)
    | BulletList [[Block]]  -- ^ Bullet list (list of items, each
                            -- a list of blocks)
    | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
                            -- Each list item is a pair consisting of a
                            -- term (a list of inlines) and one or more
                            -- definitions (each a list of blocks)
    | Header Int Attr [Inline] -- ^ Header - level (integer) and text (inlines)
    | HorizontalRule        -- ^ Horizontal rule
    | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table,
                            -- with caption, column alignments (required),
                            -- relative column widths (0 = default),
                            -- column headers (each a list of blocks), and
                            -- rows (each a list of lists of blocks)
    | Div Attr [Block]      -- ^ Generic block container with attributes
    | Null                  -- ^ Nothing
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Link target (URL, title).
type Target = (String, String)

-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Inline elements.
data Inline
    = Str String            -- ^ Text (string)
    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
    | Code Attr String      -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | Math MathType String  -- ^ TeX math (literal)
    | RawInline Format String -- ^ Raw inline
    | Link Attr [Inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
    | Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [Block]          -- ^ Footnote or endnote
    | Span Attr [Inline]    -- ^ Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data Citation = Citation { citationId      :: String
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

-- derive generic instances of FromJSON, ToJSON:

jsonOpts :: Aeson.Options
jsonOpts = Aeson.defaultOptions{
                          Aeson.fieldLabelModifier = id
                        , Aeson.constructorTagModifier = id
                        , Aeson.allNullaryToStringTag = False
                        , Aeson.omitNothingFields = False
                        , Aeson.sumEncoding = Aeson.TaggedObject "t" "c"
                        }

#if MIN_VERSION_aeson(1,0,0)
toJSON' :: (Generic a, Aeson.GToJSON Aeson.Zero (Rep a))
        => a -> Aeson.Value
#else
toJSON' :: (Generic a, Aeson.GToJSON (Rep a))
        => a -> Aeson.Value
#endif
toJSON' = Aeson.genericToJSON jsonOpts

#if MIN_VERSION_aeson(1,0,0)
parseJSON' :: (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a))
           => Aeson.Value -> Aeson.Parser a
#else
parseJSON' :: (Generic a, Aeson.GFromJSON (Rep a))
           => Aeson.Value -> Aeson.Parser a
#endif
parseJSON' = Aeson.genericParseJSON jsonOpts

instance FromJSON MetaValue
  where parseJSON = parseJSON'
instance ToJSON MetaValue where
  toJSON (MetaMap mp) =
    object [ "t" .= String "MetaMap"
           , "c" .= mp
           ]
  toJSON (MetaList lst) =
    object [ "t" .= String "MetaList"
           , "c" .= lst
           ]
  toJSON (MetaBool bool) =
    object [ "t" .= String "MetaBool"
           , "c" .= bool
           ]
  toJSON (MetaString s) =
    object [ "t" .= String "MetaString"
           , "c" .= s
           ]
  toJSON (MetaInlines ils) =
    object [ "t" .= String "MetaInlines"
           , "c" .= ils
           ]
  toJSON (MetaBlocks blks) =
    object [ "t" .= String "MetaBlocks"
           , "c" .= blks
           ]

instance FromJSON Meta
  where parseJSON = parseJSON'
instance ToJSON Meta where
  toJSON meta = object [ "unMeta" .= unMeta meta ]

instance FromJSON CitationMode
  where parseJSON = parseJSON'
instance ToJSON CitationMode where
  toJSON cmode =
    object [ "t" .= String s
           , "c" .= Aeson.emptyArray
           ]
    where s = case cmode of
            AuthorInText   -> "AuthorInText"
            SuppressAuthor -> "SuppressAuthor"
            NormalCitation -> "NormalCitation"


instance FromJSON Citation
  where parseJSON = parseJSON'
instance ToJSON Citation where
  toJSON cit =
    object [ "citationId"      .= citationId cit
           , "citationPrefix"  .= citationPrefix cit
           , "citationSuffix"  .= citationSuffix cit
           , "citationMode"    .= citationMode cit
           , "citationNoteNum" .= citationNoteNum cit
           , "citationHash"    .= citationHash cit
           ]

instance FromJSON QuoteType
  where parseJSON = parseJSON'
instance ToJSON QuoteType where
  toJSON qtype = object [ "t" .= String s
                        , "c" .= Aeson.emptyArray
                        ]
    where s = case qtype of
            SingleQuote -> "SingleQuote"
            DoubleQuote -> "DoubleQuote"


instance FromJSON MathType
  where parseJSON = parseJSON'
instance ToJSON MathType where
  toJSON mtype = object [ "t" .= String s
                        , "c" .= Aeson.emptyArray
                        ]
    where s = case mtype of
            DisplayMath -> "DisplayMath"
            InlineMath  -> "InlineMath"
  
instance FromJSON ListNumberStyle
  where parseJSON = parseJSON'
instance ToJSON ListNumberStyle where
  toJSON lsty = object [ "t" .= String s
                       , "c" .= Aeson.emptyArray
                       ]
    where s = case lsty of
            DefaultStyle -> "DefaultStyle"
            Example      -> "Example"
            Decimal      -> "Decimal"
            LowerRoman   -> "LowerRoman"
            UpperRoman   -> "UpperRoman"
            LowerAlpha   -> "LowerAlpha"
            UpperAlpha   -> "UpperAlpha"

instance FromJSON ListNumberDelim
  where parseJSON = parseJSON'
instance ToJSON ListNumberDelim where
  toJSON delim = object [ "t" .= String s
                        , "c" .= Aeson.emptyArray
                        ]
    where s = case delim of
            DefaultDelim -> "DefaultDelim"
            Period       -> "Period"
            OneParen     -> "OneParen"
            TwoParens    -> "TwoParens"

instance FromJSON Alignment
  where parseJSON = parseJSON'
instance ToJSON Alignment
  where toJSON = toJSON'

instance FromJSON Inline
  where parseJSON = parseJSON'

instance ToJSON Inline where
  toJSON (Str s) =
    object [ "t" .= String "Str"
           , "c" .= s
           ]
  toJSON (Emph ils) =
    object [ "t" .= String "Emph"
           , "c" .= ils
           ]
  toJSON (Strong ils) =
    object [ "t" .= String "Strong"
           , "c" .= ils
           ]
  toJSON (Strikeout ils) =
    object [ "t" .= String "Strikeout"
           , "c" .= ils
           ]
  toJSON (Superscript ils) =
    object [ "t" .= String "Superscript"
           , "c" .= ils
           ]
  toJSON (Subscript ils) =
    object [ "t" .= String "Subscript"
           , "c" .= ils
           ]
  toJSON (SmallCaps ils) =
    object [ "t" .= String "SmallCaps"
           , "c" .= ils
           ]
  toJSON (Quoted qtype ils) =
    object [ "t" .= String "Quoted"
           , "c" .= Array [ toJSON qtype
                          , toJSON ils
                          ]
           ]
  toJSON (Cite cits ils) =
    object [ "t" .= String "Cite"
           , "c" .= Array [ toJSON cits
                          , toJSON ils
                          ]
           ]
  toJSON (Code attr s) =
    object [ "t"  .= String "Code"
           , "c"  .= Array [ toJSON attr
                           , toJSON s
                           ]
           ]
  toJSON Space =
    object [ "t" .= String "Space"
           , "c" .= Aeson.emptyArray
           ]
  toJSON SoftBreak =
    object [ "t" .= String "SoftBreak"
           , "c" .= Aeson.emptyArray
           ]
  toJSON LineBreak =
    object [ "t" .= String "LineBreak"
           , "c" .= Aeson.emptyArray
           ]
  toJSON (Math mtype s) =
    object [ "t" .= String "Math"
           , "c" .= Array [ toJSON mtype
                          , toJSON s
                          ]
           ]
  toJSON (RawInline fmt s) =
    object [ "t" .= String "RawInline"
           , "c" .= Array [ toJSON fmt
                          , toJSON s
                          ]
           ]
  toJSON (Link attr ils target) =
    object [ "t" .= String "Link"
           , "c" .= Array [ toJSON attr
                          , toJSON ils
                          , toJSON target
                          ]
           ]
  toJSON (Image attr ils target) =
    object [ "t" .= String "Image"
           , "c" .= Array [ toJSON attr
                          , toJSON ils
                          , toJSON target
                          ]
           ]

  toJSON (Note blks) =
    object [ "t" .= String "Note"
           , "c" .= blks
           ]
  toJSON (Span attr ils) =
    object [ "t" .= String "Span"
           , "c" .= Array [ toJSON attr
                          , toJSON ils
                          ]
           ]

instance FromJSON Block
  where parseJSON = parseJSON'
instance ToJSON Block where
  toJSON (Plain ils) =
    object [ "t" .= String "Plain"
           , "c" .= ils
           ]
  toJSON (Para ils) =
    object [ "t" .= String "Para"
           , "c" .= ils
           ]
  toJSON (CodeBlock attr s) =
    object [ "t" .= String "CodeBlock"
           , "c" .= Array [ toJSON attr
                          , toJSON s
                          ]
           ]
  toJSON (RawBlock fmt s) =
    object [ "t" .= String "RawBlock"
           , "c" .= Array [ toJSON fmt
                          , toJSON s
                          ]
           ]
  toJSON (BlockQuote blks) =
    object [ "t" .= String "BlockQuote"
           , "c" .= blks
           ]

  toJSON (OrderedList listAttrs blksList) =
    object [ "t" .= String "OrderedList"
           , "c" .= Array [ toJSON listAttrs
                          , toJSON blksList
                          ]
           ]
  toJSON (BulletList blksList) =
    object [ "t" .= String "BulletList"
           , "c" .= blksList
           ]
  toJSON (DefinitionList defs) =
    object [ "t" .= String "DefinitionList"
           , "c" .= defs
           ]
  toJSON (Header n attr ils) =
    object [ "t" .= String "Header"
           , "c" .= Array [ toJSON n
                          , toJSON attr
                          , toJSON ils
                          ]
           ]
  toJSON HorizontalRule =
    object [ "t" .= String "HorizontalRule"
           , "c" .= Aeson.emptyArray
           ]
  toJSON (Table caption aligns widths cells rows) =
    object [ "t" .= String "Table"
           , "c" .= Array [ toJSON caption
                          , toJSON aligns
                          , toJSON widths
                          , toJSON cells
                          , toJSON rows
                          ]
           ]
  toJSON (Div attr blks) =
    object [ "t" .= String "Div"
           , "c" .= blks
           ]
  toJSON Null =
    object [ "t" .= String "Null"
           , "c" .= Aeson.emptyArray
           ]

instance FromJSON Pandoc
  where parseJSON = parseJSON'
instance ToJSON Pandoc where
  toJSON (Pandoc meta blks) =
    Array [ toJSON meta
          , toJSON blks
          ]

-- Instances for deepseq
#if MIN_VERSION_base(4,8,0)
instance NFData MetaValue
instance NFData Meta
instance NFData Citation
instance NFData Alignment
instance NFData Inline
instance NFData MathType
instance NFData Format
instance NFData CitationMode
instance NFData QuoteType
instance NFData ListNumberDelim
instance NFData ListNumberStyle
instance NFData Block
instance NFData Pandoc
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
