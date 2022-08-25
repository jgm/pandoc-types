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
module Text.Pandoc.Definition.Misc
  ( ListAttributes
  , ListNumberStyle(..)
  , ListNumberDelim(..)
  , Format(..)
  , Attr
  , nullAttr
  , RowHeadColumns(..)
  , Alignment(..)
  , ColWidth(..)
  , ColSpec
  , RowSpan(..)
  , ColSpan(..)
  , QuoteType(..)
  , Target
  , MathType(..)
  , CitationMode(..)
  , pandocTypesVersion
  ) where

import Data.Generics (Data, Typeable)
import Data.Aeson hiding (Null)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.String
import Control.DeepSeq
import Paths_pandoc_types (version)
import Data.Version (Version)

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

-- | The number of rows occupied by a cell; the height of a cell.
newtype RowSpan = RowSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | The number of columns occupied by a cell; the width of a cell.
newtype ColSpan = ColSpan Int
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, Num, Enum, ToJSON, FromJSON)

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Link target (URL, title).
type Target = (Text, Text)


-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data CitationMode = AuthorInText | SuppressAuthor | NormalCitation
                    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)


-- ToJSON/FromJSON instances. Some are defined by hand so that we have
-- more control over the format.

$(let jsonOpts = defaultOptions
        { allNullaryToStringTag = False
        , sumEncoding = TaggedObject { tagFieldName = "t", contentsFieldName = "c" }
        }
  in fmap concat $ traverse (deriveJSON jsonOpts)
     [ ''CitationMode
     , ''QuoteType
     , ''MathType
     , ''ListNumberStyle
     , ''ListNumberDelim
     , ''Alignment
     , ''ColWidth
     ])

-- Instances for deepseq
instance NFData Alignment
instance NFData RowSpan
instance NFData ColSpan
instance NFData MathType
instance NFData Format
instance NFData CitationMode
instance NFData QuoteType
instance NFData ListNumberDelim
instance NFData ListNumberStyle
instance NFData ColWidth
instance NFData RowHeadColumns

pandocTypesVersion :: Version
pandocTypesVersion = version
