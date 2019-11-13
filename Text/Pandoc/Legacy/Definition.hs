{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Text.Pandoc.Legacy.Definition
  ( D.Pandoc(..)
  , D.Meta
  , pattern Meta
  , unMeta
  , D.MetaValue ( MetaList
                , MetaBool
                , MetaInlines
                , MetaBlocks
                )
  , pattern MetaMap
  , pattern MetaString
  , D.nullMeta
  , D.isNullMeta
  , lookupMeta
  , D.docTitle
  , D.docAuthors
  , D.docDate
  , D.Block ( Plain
            , Para
            , LineBlock
            , BlockQuote
            , OrderedList
            , BulletList
            , DefinitionList
            , HorizontalRule
            , Table
            , Null
            )
  , pattern CodeBlock
  , pattern RawBlock
  , pattern Header
  , pattern Div
  , D.Inline ( Emph
             , Strong
             , Strikeout
             , Superscript
             , Subscript
             , SmallCaps
             , Quoted
             , Cite
             , Space
             , SoftBreak
             , LineBreak
             , Note
             )
  , pattern Str
  , pattern Code
  , pattern Math
  , pattern RawInline
  , pattern Link
  , pattern Image
  , pattern Span
  , D.Alignment(..)
  , D.ListAttributes
  , D.ListNumberStyle(..)
  , D.ListNumberDelim(..)
  , D.Format
  , pattern Format
  , Attr
  , nullAttr
  , D.TableCell
  , D.QuoteType(..)
  , Target
  , D.MathType(..)
  , D.Citation
  , pattern Citation
  , citationId
  , citationPrefix
  , citationSuffix
  , citationMode
  , citationNoteNum
  , citationHash
  , D.CitationMode(..)
  , D.pandocTypesVersion
  ) where

import qualified Text.Pandoc.Definition as D
import Text.Pandoc.Definition
  ( MetaValue ( MetaList
              , MetaBool
              , MetaInlines
              , MetaBlocks
              )
  , Block ( Plain
          , Para
          , LineBlock
          , BlockQuote
          , OrderedList
          , BulletList
          , DefinitionList
          , HorizontalRule
          , Table
          , Null
          )
  , Inline ( Emph
           , Strong
           , Strikeout
           , Superscript
           , Subscript
           , SmallCaps
           , Quoted
           , Cite
           , Space
           , SoftBreak
           , LineBreak
           , Note
           )
  )
import qualified Data.Map as M
import qualified Data.Text as T

unpack2 :: (T.Text, T.Text) -> (String, String)
unpack2 (x, y) = (T.unpack x, T.unpack y)

pack2 :: (String, String) -> (T.Text, T.Text)
pack2 (x, y) = (T.pack x, T.pack y)

toLegacyMap :: M.Map T.Text a -> M.Map String a
toLegacyMap = M.mapKeys T.unpack

fromLegacyMap :: M.Map String a -> M.Map T.Text a
fromLegacyMap = M.mapKeys T.pack

toLegacyAttr :: D.Attr -> Attr
toLegacyAttr (a, b, c) = (T.unpack a, map T.unpack b, map unpack2 c)

fromLegacyAttr :: Attr -> D.Attr
fromLegacyAttr (a, b, c) = (T.pack a, map T.pack b, map pack2 c)

pattern Meta :: M.Map String D.MetaValue -> D.Meta
pattern Meta {unMeta} <- D.Meta (toLegacyMap -> unMeta)
  where
    Meta = D.Meta . fromLegacyMap

{-# COMPLETE Meta :: D.Meta #-}

pattern MetaMap :: M.Map String D.MetaValue -> D.MetaValue
pattern MetaMap x <- D.MetaMap (toLegacyMap -> x)
  where
    MetaMap = D.MetaMap . fromLegacyMap

pattern MetaString :: String -> D.MetaValue
pattern MetaString x <- D.MetaString (T.unpack -> x)
  where
    MetaString = D.MetaString . T.pack

{-# COMPLETE MetaList, MetaBool, MetaInlines, MetaBlocks, MetaMap, MetaString :: D.MetaValue #-}

lookupMeta :: String -> D.Meta -> Maybe D.MetaValue
lookupMeta = D.lookupMeta . T.pack

pattern CodeBlock :: Attr -> String -> D.Block
pattern CodeBlock a s <- D.CodeBlock (toLegacyAttr -> a) (T.unpack -> s)
  where
    CodeBlock a = D.CodeBlock (fromLegacyAttr a) . T.pack

pattern RawBlock :: D.Format -> String -> D.Block
pattern RawBlock f s <- D.RawBlock f (T.unpack -> s)
  where
    RawBlock f = D.RawBlock f . T.pack

pattern Header :: Int -> Attr -> [D.Inline] -> D.Block
pattern Header n a i <- D.Header n (toLegacyAttr -> a) i
  where
    Header n = D.Header n . fromLegacyAttr

pattern Div :: Attr -> [D.Block] -> D.Block
pattern Div a b <- D.Div (toLegacyAttr -> a) b
  where
    Div = D.Div . fromLegacyAttr

{-# COMPLETE Plain, Para, LineBlock, BlockQuote, OrderedList,
             BulletList, DefinitionList, HorizontalRule, Table,
             Null, CodeBlock, RawBlock, Header, Div :: D.Block #-}

pattern Str :: String -> D.Inline
pattern Str s <- D.Str (T.unpack -> s)
  where
    Str = D.Str . T.pack

pattern Code :: Attr -> String -> D.Inline
pattern Code a s <- D.Code (toLegacyAttr -> a) (T.unpack -> s)
  where
    Code a = D.Code (fromLegacyAttr a) . T.pack

pattern Math :: D.MathType -> String -> D.Inline
pattern Math m s <- D.Math m (T.unpack -> s)
  where
    Math m = D.Math m . T.pack

pattern RawInline :: D.Format -> String -> D.Inline
pattern RawInline f s <- D.RawInline f (T.unpack -> s)
  where
    RawInline f = D.RawInline f . T.pack

pattern Link :: Attr -> [D.Inline] -> Target -> D.Inline
pattern Link a i t <- D.Link (toLegacyAttr -> a) i (unpack2 -> t)
  where
    Link a i = D.Link (fromLegacyAttr a) i . pack2

pattern Image :: Attr -> [D.Inline] -> Target -> D.Inline
pattern Image a i t <- D.Image (toLegacyAttr -> a) i (unpack2 -> t)
  where
    Image a i = D.Image (fromLegacyAttr a) i . pack2

pattern Span :: Attr -> [D.Inline] -> D.Inline
pattern Span a i <- D.Span (toLegacyAttr -> a) i
  where
    Span = D.Span . fromLegacyAttr


{-# COMPLETE Emph, Strong, Strikeout, Superscript, Subscript,
             SmallCaps, Quoted, Cite, Space, SoftBreak, LineBreak,
             Note, Str, Code, Math, RawInline, Link, Image, Span :: Inline #-}

pattern Format :: String -> D.Format
pattern Format x <- D.Format (T.unpack -> x)
  where
    Format x = D.Format $ T.pack x

{-# COMPLETE Format :: D.Format #-}

type Attr = (String, [String], [(String, String)])

nullAttr :: Attr
nullAttr = ("", [], [])

type Target = (String, String)

pattern Citation
  :: String
  -> [D.Inline]
  -> [D.Inline]
  -> D.CitationMode
  -> Int
  -> Int
  -> D.Citation
pattern Citation
  { citationId
  , citationPrefix
  , citationSuffix
  , citationMode
  , citationNoteNum
  , citationHash } <- D.Citation (T.unpack -> citationId)
                                 citationPrefix
                                 citationSuffix
                                 citationMode
                                 citationNoteNum
                                 citationHash
  where
    Citation = D.Citation . T.pack

{-# COMPLETE Citation :: D.Citation #-}
