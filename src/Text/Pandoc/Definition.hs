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
module Text.Pandoc.Definition ( Pandoc(..)
                              , Meta(..)
                              , MetaValue(..)
                              , MetaValueF(..)
                              , nullMeta
                              , isNullMeta
                              , lookupMeta
                              , docTitle
                              , docAuthors
                              , docDate
                              , Block(..)
                              , BlockF(..)
                              , pattern SimpleFigure
                              , Inline(..)
                              , InlineF(..)
                              , ListAttributes
                              , ListNumberStyle(..)
                              , ListNumberDelim(..)
                              , Format(..)
                              , Attr
                              , nullAttr
                              , Caption
                              , CaptionF(..)
                              , ShortCaption
                              , ShortCaptionF
                              , RowHeadColumns(..)
                              , Alignment(..)
                              , ColWidth(..)
                              , ColSpec
                              , Row
                              , RowF(..)
                              , TableHead
                              , TableHeadF(..)
                              , TableBody
                              , TableBodyF(..)
                              , TableFoot
                              , TableFootF(..)
                              , Cell
                              , CellF(..)
                              , RowSpan(..)
                              , ColSpan(..)
                              , QuoteType(..)
                              , Target
                              , MathType(..)
                              , Citation
                              , CitationF(..)
                              , CitationMode(..)
                              , pandocTypesVersion
                              ) where

import Data.Generics (Data, Typeable)
import Data.Aeson hiding (Null)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Version (versionBranch)
import Data.Semigroup (Semigroup(..))

import Text.Pandoc.Definition.Functors

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

newtype MetaValue = MetaValue
  { unMetaValue :: MetaValueF Inline Block MetaValue }
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
  case unMetaValue <$> lookupMeta "title" meta of
         Just (MetaString s)           -> [Inline $ Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Block (Plain ils)]) -> ils
         Just (MetaBlocks [Block (Para ils)])  -> ils
         _                             -> []

-- | Extract document authors from metadata; works just like the old
-- @docAuthors@.
docAuthors :: Meta -> [[Inline]]
docAuthors meta =
  case unMetaValue <$> lookupMeta "author" meta of
        Just (MetaString s)    -> [[Inline $ Str s]]
        Just (MetaInlines ils) -> [ils]
        Just (MetaList   ms)   -> [ils | MetaValue (MetaInlines ils) <- ms] ++
                                  [ils | MetaValue (MetaBlocks [Block (Plain ils)]) <- ms] ++
                                  [ils | MetaValue (MetaBlocks [Block (Para ils)])  <- ms] ++
                                  [[Inline $ Str x] | MetaValue (MetaString x) <- ms]
        _                      -> []

-- | Extract date from metadata; works just like the old @docDate@.
docDate :: Meta -> [Inline]
docDate meta =
  case unMetaValue <$> lookupMeta "date" meta of
         Just (MetaString s)           -> [Inline $ Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Block (Plain ils)]) -> ils
         Just (MetaBlocks [Block (Para ils)])  -> ils
         _                             -> []

-- | A table row.
type Row = RowF Block

-- | The head of a table.
type TableHead = TableHeadF Block

-- | A body of a table, with an intermediate head, intermediate body,
-- and the specified number of row header columns in the intermediate
-- body.
type TableBody = TableBodyF Block

-- | The foot of a table.
type TableFoot = TableFootF Block

-- | A short caption, for use in, for instance, lists of figures.
type ShortCaption = ShortCaptionF Inline

-- | The caption of a table, with an optional short caption.
type Caption = CaptionF Inline Block

-- | A table cell.
type Cell = CellF Block

-- | Block element.
newtype Block = Block { unBlock :: BlockF Inline Block }
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


-- | Inline elements.
newtype Inline = Inline { unInline :: InlineF Block Inline }
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

type Citation = CitationF Inline


-- ToJSON/FromJSON instances. Some are defined by hand so that we have
-- more control over the format.

$(let jsonOpts = defaultOptions
        { allNullaryToStringTag = False
        , sumEncoding = TaggedObject { tagFieldName = "t", contentsFieldName = "c" }
        }
  in fmap concat $ traverse (deriveJSON jsonOpts)
     [ ''MetaValue
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
instance NFData Meta
instance NFData MetaValue
instance NFData Inline
instance NFData Block
instance NFData Pandoc
