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
  ( MetaValue(..)
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
import Data.Aeson.TH (deriveJSON)
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq

import Text.Pandoc.Definition.Misc

data MetaValue inline block metaValue
               = MetaMap (M.Map Text metaValue)
               | MetaList [metaValue]
               | MetaBool Bool
               | MetaString Text
               | MetaInlines [inline]
               | MetaBlocks [block]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A table row.
data Row block = Row Attr [Cell block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | The head of a table.
data TableHead block = TableHead Attr [Row block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | A body of a table, with an intermediate head, intermediate body,
-- and the specified number of row header columns in the intermediate
-- body.
data TableBody block = TableBody Attr RowHeadColumns [Row block] [Row block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | The foot of a table.
data TableFoot block = TableFoot Attr [Row block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | A short caption, for use in, for instance, lists of figures.
type ShortCaption inline = [inline]

-- | The caption of a table, with an optional short caption.
data Caption inline block = Caption (Maybe (ShortCaption inline)) [block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | A table cell.
data Cell block = Cell Attr Alignment RowSpan ColSpan [block]
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

-- | Block element
data Block inline block
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
    | Table Attr (Caption inline block) [ColSpec] (TableHead block) [TableBody block] (TableFoot block)
    -- | Generic block container with attributes
    | Div Attr [block]
    -- | Nothing
    | Null
    deriving ( Eq, Ord, Read, Show, Typeable, Data, Generic
             , Functor, Foldable, Traversable
             )


-- | Inline elements.
data Inline block inline
    = Str Text            -- ^ Text (string)
    | Emph [inline]         -- ^ Emphasized text (list of inlines)
    | Underline [inline]    -- ^  Underlined text (list of inlines)
    | Strong [inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation inline]  [inline] -- ^ Citation (list of inlines)
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

data Citation inline = Citation
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

instance Eq inline => Ord (Citation inline) where
    compare = comparing citationHash


-- ToJSON/FromJSON instances. Some are defined by hand so that we have
-- more control over the format.

$(let jsonOpts = defaultOptions
        { allNullaryToStringTag = False
        , sumEncoding = TaggedObject { tagFieldName = "t", contentsFieldName = "c" }
        }
  in fmap concat $ traverse (deriveJSON jsonOpts)
     [ ''MetaValue
     , ''Citation
     , ''Row
     , ''Caption
     , ''TableHead
     , ''TableBody
     , ''TableFoot
     , ''Cell
     , ''Inline
     , ''Block
     ])

-- Instances for deepseq
instance (NFData block, NFData inline, NFData metaValue) => NFData (MetaValue inline block metaValue)
instance NFData inline => NFData (Citation inline)
instance NFData block => NFData (Cell block)
instance NFData block => NFData (Row block)
instance NFData block => NFData (TableHead block)
instance NFData block => NFData (TableBody block)
instance NFData block => NFData (TableFoot block)
instance (NFData block, NFData inline) => NFData (Caption block inline)
instance (NFData block, NFData inline) => NFData (Inline block inline)
instance (NFData inline, NFData block) => NFData (Block inline block)
