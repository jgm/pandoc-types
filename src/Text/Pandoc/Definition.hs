{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric,
    FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards, CPP,
    TemplateHaskell , PatternSynonyms, ViewPatterns, StrictData #-}

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
                              , MetaValue
                                ( ..
                                , MetaMap
                                , MetaList
                                , MetaBool
                                , MetaString
                                , MetaInlines
                                , MetaBlocks
                                )
                              , nullMeta
                              , isNullMeta
                              , lookupMeta
                              , docTitle
                              , docAuthors
                              , docDate
                              , Block
                                ( ..
                                , Plain
                                , Para
                                , LineBlock
                                , CodeBlock
                                , RawBlock
                                , BlockQuote
                                , OrderedList
                                , BulletList
                                , DefinitionList
                                , Header
                                , HorizontalRule
                                , Table
                                , Div
                                , Null
                                )
                              , pattern SimpleFigure
                              , Inline
                                ( ..
                                , Str
                                , Emph
                                , Underline
                                , Strong
                                , Strikeout
                                , Superscript
                                , Subscript
                                , SmallCaps
                                , Quoted
                                , Cite
                                , Code
                                , Space
                                , SoftBreak
                                , LineBreak
                                , Math
                                , RawInline
                                , Link
                                , Image
                                , Note
                                , Span
                                )
                              , ListAttributes
                              , ListNumberStyle(..)
                              , ListNumberDelim(..)
                              , Format(..)
                              , Attr
                              , nullAttr
                              , Caption, pattern F.Caption
                              , ShortCaption
                              , RowHeadColumns(..)
                              , Alignment(..)
                              , ColWidth(..)
                              , ColSpec
                              , Row, pattern F.Row
                              , TableHead, pattern F.TableHead
                              , TableBody, pattern F.TableBody
                              , TableFoot, pattern F.TableFoot
                              , Cell, pattern F.Cell
                              , RowSpan(..)
                              , ColSpan(..)
                              , QuoteType(..)
                              , Target
                              , MathType(..)
                              , Citation, pattern F.Citation
                              , F.citationId
                              , F.citationPrefix
                              , F.citationSuffix
                              , F.citationMode
                              , F.citationNoteNum
                              , F.citationHash
                              , CitationMode(..)
                              , pandocTypesVersion
                              ) where

import Data.Generics (Data, Typeable)
import Data.Aeson hiding (Null)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Version (versionBranch)
import Data.Semigroup (Semigroup(..))
import Control.Arrow (second)

import qualified Text.Pandoc.Definition.Functors as F
import Text.Pandoc.Definition.Misc

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
  { unMetaValue :: F.MetaValue Inline Block MetaValue }
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

{-# COMPLETE MetaMap
           , MetaList
           , MetaBool
           , MetaString
           , MetaInlines
           , MetaBlocks
           :: MetaValue #-}

pattern MetaMap :: M.Map Text MetaValue -> MetaValue
pattern MetaMap m = MetaValue (F.MetaMap m)

pattern MetaList :: [MetaValue] -> MetaValue
pattern MetaList l = MetaValue (F.MetaList l)

pattern MetaBool :: Bool -> MetaValue
pattern MetaBool b = MetaValue (F.MetaBool b)

pattern MetaString :: Text -> MetaValue
pattern MetaString t = MetaValue (F.MetaString t)

pattern MetaInlines :: [Inline] -> MetaValue
pattern MetaInlines is = MetaValue (F.MetaInlines is)

pattern MetaBlocks :: [Block] -> MetaValue
pattern MetaBlocks bs = MetaValue (F.MetaBlocks bs)

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

-- | A table row.
type Row = F.Row Block

-- | The head of a table.
type TableHead = F.TableHead Block

-- | A body of a table, with an intermediate head, intermediate body,
-- and the specified number of row header columns in the intermediate
-- body.
type TableBody = F.TableBody Block

-- | The foot of a table.
type TableFoot = F.TableFoot Block

-- | A short caption, for use in, for instance, lists of figures.
type ShortCaption = F.ShortCaption Inline

-- | The caption of a table, with an optional short caption.
type Caption = F.Caption Inline Block

-- | A table cell.
type Cell = F.Cell Block

-- | Block element.
newtype Block = Block { unBlock :: F.Block Inline Block }
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

{-# COMPLETE Plain
           , Para
           , LineBlock
           , CodeBlock
           , RawBlock
           , BlockQuote
           , OrderedList
           , BulletList
           , DefinitionList
           , Header
           , HorizontalRule
           , Table
           , Div
           , Null
           :: Block #-}

-- | Plain text, not a paragraph
pattern Plain :: [Inline] -> Block
pattern Plain is = Block (F.Plain is)

-- | Paragraph
pattern Para :: [Inline] -> Block
pattern Para is = Block (F.Para is)

-- | Multiple non-breaking lines
pattern LineBlock :: [[Inline]] -> Block
pattern LineBlock iss = Block (F.LineBlock iss)

-- | Code block (literal) with attributes
pattern CodeBlock :: Attr -> Text -> Block
pattern CodeBlock a t = Block (F.CodeBlock a t)

-- | Raw block
pattern RawBlock :: Format -> Text -> Block
pattern RawBlock f t = Block (F.RawBlock f t)

-- | Block quote (list of blocks)
pattern BlockQuote :: [Block] -> Block
pattern BlockQuote bs = Block (F.BlockQuote bs)

-- | Ordered list (attributes and a list of items, each a list of
-- blocks)
pattern OrderedList :: ListAttributes -> [[Block]] -> Block
pattern OrderedList as bs = Block (F.OrderedList as bs)

-- | Bullet list (list of items, each a list of blocks)
pattern BulletList :: [[Block]] -> Block
pattern BulletList bss = Block (F.BulletList bss)

-- | Definition list. Each list item is a pair consisting of a
-- term (a list of inlines) and one or more definitions (each a
-- list of blocks)
pattern DefinitionList :: [([Inline], [[Block]])] -> Block
pattern DefinitionList l = Block (F.DefinitionList l)

-- | Header - level (integer) and text (inlines)
pattern Header :: Int -> Attr -> [Inline] -> Block
pattern Header i a is = Block (F.Header i a is)

-- | Horizontal rule
pattern HorizontalRule :: Block
pattern HorizontalRule = Block F.HorizontalRule

-- | Table, with attributes, caption, optional short caption,
-- column alignments and widths (required), table head, table
-- bodies, and table foot
pattern Table :: Attr -> Caption -> [ColSpec] -> TableHead -> [TableBody] -> TableFoot -> Block
pattern Table i c css th tbs tf = Block (F.Table i c css th tbs tf)

-- | Generic block container with attributes
pattern Div :: Attr -> [Block] -> Block
pattern Div as bs = Block (F.Div as bs)

-- | Nothing
pattern Null :: Block
pattern Null = Block F.Null

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

{-# COMPLETE Str
           , Emph
           , Underline
           , Strong
           , Strikeout
           , Superscript
           , Subscript
           , SmallCaps
           , Quoted
           , Cite
           , Code
           , Space
           , SoftBreak
           , LineBreak
           , Math
           , RawInline
           , Link
           , Image
           , Note
           , Span
           :: Inline #-}

-- | Inline elements.
newtype Inline = Inline { unInline :: F.Inline Block Inline }
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Text (string)
pattern Str :: Text -> Inline
pattern Str t = Inline (F.Str t)

-- | Emphasized text (list of inlines)
pattern Emph :: [Inline] -> Inline
pattern Emph is = Inline (F.Emph is)

-- | Underlined text (list of inlines)
pattern Underline :: [Inline] -> Inline
pattern Underline is = Inline (F.Underline is)

-- | Strongly emphasized text (list of inlines)
pattern Strong :: [Inline] -> Inline
pattern Strong is = Inline (F.Strong is)

-- | Strikeout text (list of inlines)
pattern Strikeout :: [Inline] -> Inline
pattern Strikeout is = Inline (F.Strikeout is)

-- | Superscripted text (list of inlines)
pattern Superscript :: [Inline] -> Inline
pattern Superscript is = Inline (F.Superscript is)

-- | Subscripted text (list of inlines)
pattern Subscript :: [Inline] -> Inline
pattern Subscript is = Inline (F.Subscript is)

-- | Small caps text (list of inlines)
pattern SmallCaps :: [Inline] -> Inline
pattern SmallCaps is = Inline (F.SmallCaps is)

-- | Quoted text (list of inlines)
pattern Quoted :: QuoteType -> [Inline] -> Inline
pattern Quoted qt is = Inline (F.Quoted qt is)

-- | Citation (list of inlines)
pattern Cite :: [F.Citation Inline] -> [Inline] -> Inline
pattern Cite cs is = Inline (F.Cite cs is)

-- | Inline code (literal)
pattern Code :: Attr -> Text -> Inline
pattern Code a is = Inline (F.Code a is)

-- | Inter-word space
pattern Space :: Inline
pattern Space = Inline F.Space

-- | Soft line break
pattern SoftBreak :: Inline
pattern SoftBreak = Inline F.SoftBreak

-- | Hard line break
pattern LineBreak :: Inline
pattern LineBreak = Inline F.LineBreak

-- | TeX math (literal)
pattern Math :: MathType -> Text -> Inline
pattern Math mt t = Inline (F.Math mt t)

-- | Raw inline
pattern RawInline :: Format -> Text -> Inline
pattern RawInline f t = Inline (F.RawInline f t)

-- | Hyperlink: alt text (list of inlines), target
pattern Link :: Attr -> [Inline] -> Target -> Inline
pattern Link a is t = Inline (F.Link a is t)

-- | Image:  alt text (list of inlines), target
pattern Image :: Attr -> [Inline] -> Target -> Inline
pattern Image a is t = Inline (F.Image a is t)

-- | Footnote or endnote
pattern Note :: [Block] -> Inline
pattern Note bs = Inline (F.Note bs)

-- | Generic inline container with attributes
pattern Span :: Attr -> [Inline] -> Inline
pattern Span a is = Inline (F.Span a is)


type Citation = F.Citation Inline


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
