{-# LANGUAGE CPP, DeriveDataTypeable #-}

#ifdef GENERICS
{-# LANGUAGE DeriveGeneric #-}
#endif

{-
Copyright (C) 2006-2013 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Definition
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

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
                              , Attr
                              , nullAttr
                              , TableCell
                              , Format
                              , QuoteType(..)
                              , Target
                              , MathType(..)
                              , Citation(..)
                              , CitationMode(..)
                              ) where

import Data.Generics (Data, Typeable)
import Data.Ord (comparing)
import Control.Monad (guard)
import qualified Data.Map as M

#ifdef GENERICS
import GHC.Generics (Generic)
#define GENERIC , Generic
#else
#define GENERIC
#endif

data Pandoc = Pandoc { docMeta :: Meta
                     , docBody :: [Block]
                     } deriving (Eq, Ord, Read, Show, Typeable, Data GENERIC)

-- | Metadata for the document:  title, authors, date.
newtype Meta = Meta { unMeta :: M.Map String MetaValue }
               deriving (Eq, Ord, Show, Read, Typeable, Data GENERIC)

data MetaValue = MetaMap (M.Map String MetaValue)
               | MetaList [MetaValue]
               | MetaString String
               | MetaBlocks [Block]
               deriving (Eq, Ord, Show, Read, Typeable, Data GENERIC)

nullMeta :: Meta
nullMeta = Meta M.empty

-- Helper functions to extract metadata

-- | Retrieve the metadata value for a given @key@.
lookupMeta :: String -> Meta -> Maybe MetaValue
lookupMeta key (Meta m) = M.lookup key m

extractInlines :: Block -> [Inline]
extractInlines (Plain ils) = ils
extractInlines (Para ils ) = ils
extractInlines _           = []

-- | Extract document title from metadata; works just like the old @docTitle@.
docTitle :: Meta -> [Inline]
docTitle meta =
  case lookupMeta "title" meta of
         Just (MetaBlocks bs) -> concatMap extractInlines bs
         _                    -> []

-- | Extract document authors from metadata; works just like the old
-- @docAuthors@.
docAuthors :: Meta -> [[Inline]]
docAuthors meta =
  case lookupMeta "author" meta of
        Just (MetaBlocks bs)  -> [concatMap extractInlines bs]
        Just (MetaList   ms)  -> [concatMap extractInlines bs |
                                    MetaBlocks bs <- ms]
        _                     -> []

-- | Extract date from metadata; works just like the old @docDate@.
docDate :: Meta -> [Inline]
docDate meta =
  case lookupMeta "date" meta of
         Just (MetaBlocks bs) -> concatMap extractInlines bs
         _                    -> []

-- | Alignment of a table column.
data Alignment = AlignLeft
               | AlignRight
               | AlignCenter
               | AlignDefault deriving (Eq, Ord, Show, Read, Typeable, Data GENERIC)

-- | List attributes.
type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)

-- | Style of list numbers.
data ListNumberStyle = DefaultStyle
                     | Example
                     | Decimal
                     | LowerRoman
                     | UpperRoman
                     | LowerAlpha
                     | UpperAlpha deriving (Eq, Ord, Show, Read, Typeable, Data GENERIC)

-- | Delimiter of list numbers.
data ListNumberDelim = DefaultDelim
                     | Period
                     | OneParen
                     | TwoParens deriving (Eq, Ord, Show, Read, Typeable, Data GENERIC)

-- | Attributes: identifier, classes, key-value pairs
type Attr = (String, [String], [(String, String)])

nullAttr :: Attr
nullAttr = ("",[],[])

-- | Table cells are list of Blocks
type TableCell = [Block]

-- | Formats for raw blocks
type Format = String

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
                            -- with caption, column alignments,
                            -- relative column widths (0 = default),
                            -- column headers (each a list of blocks), and
                            -- rows (each a list of lists of blocks)
    | Null                  -- ^ Nothing
    deriving (Eq, Ord, Read, Show, Typeable, Data GENERIC)

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote deriving (Show, Eq, Ord, Read, Typeable, Data GENERIC)

-- | Link target (URL, title).
type Target = (String, String)

-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath deriving (Show, Eq, Ord, Read, Typeable, Data GENERIC)

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
    | LineBreak             -- ^ Hard line break
    | Math MathType String  -- ^ TeX math (literal)
    | RawInline Format String -- ^ Raw inline
    | Link [Inline] Target  -- ^ Hyperlink: text (list of inlines), target
    | Image [Inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [Block]          -- ^ Footnote or endnote
    deriving (Show, Eq, Ord, Read, Typeable, Data GENERIC)

data Citation = Citation { citationId      :: String
                         , citationPrefix  :: [Inline]
                         , citationSuffix  :: [Inline]
                         , citationMode    :: CitationMode
                         , citationNoteNum :: Int
                         , citationHash    :: Int
                         }
                deriving (Show, Eq, Read, Typeable, Data GENERIC)

instance Ord Citation where
    compare = comparing citationHash

data CitationMode = AuthorInText | SuppressAuthor | NormalCitation
                    deriving (Show, Eq, Ord, Read, Typeable, Data GENERIC)
