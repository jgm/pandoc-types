{-# LANGUAGE FlexibleInstances #-}

module Text.Pandoc.Legacy.Builder
  ( module Text.Pandoc.Legacy.Definition
  , B.Many(..)
  , B.Inlines
  , B.Blocks
  , (<>)
  , B.singleton
  , B.toList
  , B.fromList
  , B.isNull
  , B.doc
  , ToMetaValue(..)
  , HasMeta(..)
  , B.setTitle
  , B.setAuthors
  , B.setDate
  , text
  , str
  , B.emph
  , B.strong
  , B.strikeout
  , B.superscript
  , B.subscript
  , B.smallcaps
  , B.singleQuoted
  , B.doubleQuoted
  , B.cite
  , codeWith
  , code
  , B.space
  , B.softbreak
  , B.linebreak
  , math
  , displayMath
  , rawInline
  , link
  , linkWith
  , image
  , imageWith
  , B.note
  , spanWith
  , B.trimInlines
  , B.para
  , B.plain
  , B.lineBlock
  , codeBlockWith
  , codeBlock
  , rawBlock
  , B.blockQuote
  , B.bulletList
  , B.orderedListWith
  , B.orderedList
  , B.definitionList
  , B.header
  , headerWith
  , B.horizontalRule
  , B.table
  , B.simpleTable
  , divWith
  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Legacy.Definition
import Data.Semigroup (Semigroup(..))

fromLegacyAttr :: Attr -> B.Attr
fromLegacyAttr (a, b, c) = (T.pack a, map T.pack b, map pack2 c)
  where
    pack2 (x, y) = (T.pack x, T.pack y)

class ToMetaValue a where
  toMetaValue :: a -> B.MetaValue

instance ToMetaValue B.MetaValue where
  toMetaValue = id

instance ToMetaValue B.Blocks where
  toMetaValue = B.MetaBlocks . B.toList

instance ToMetaValue B.Inlines where
  toMetaValue = MetaInlines . B.toList

instance ToMetaValue Bool where
  toMetaValue = MetaBool

instance {-# OVERLAPPING #-} ToMetaValue String where
  toMetaValue = MetaString

instance ToMetaValue a => ToMetaValue [a] where
  toMetaValue = MetaList . map toMetaValue

instance ToMetaValue a => ToMetaValue (M.Map String a) where
  toMetaValue = MetaMap . M.map toMetaValue

class HasMeta a where
  setMeta :: ToMetaValue b => String -> b -> a -> a
  deleteMeta :: String -> a -> a

instance HasMeta Meta where
  setMeta key val (B.Meta ms) = B.Meta $ M.insert (T.pack key) (toMetaValue val) ms
  deleteMeta key (B.Meta ms) = B.Meta $ M.delete (T.pack key) ms

instance HasMeta Pandoc where
  setMeta key val (Pandoc (B.Meta ms) bs) =
    Pandoc (B.Meta $ M.insert (T.pack key) (toMetaValue val) ms) bs
  deleteMeta key (Pandoc (B.Meta ms) bs) =
    Pandoc (B.Meta $ M.delete (T.pack key) ms) bs

text :: String -> B.Inlines
text = B.text . T.pack

str :: String -> B.Inlines
str = B.str . T.pack

codeWith :: Attr -> String -> B.Inlines
codeWith a = B.codeWith (fromLegacyAttr a) . T.pack

code :: String -> B.Inlines
code = B.code . T.pack

math :: String -> B.Inlines
math = B.math . T.pack

displayMath :: String -> B.Inlines
displayMath = B.displayMath . T.pack

rawInline :: String -> String -> B.Inlines
rawInline s = B.rawInline (T.pack s) . T.pack

link :: String -> String -> B.Inlines -> B.Inlines
link s = B.link (T.pack s) . T.pack

linkWith :: Attr -> String -> String -> B.Inlines -> B.Inlines
linkWith a s = B.linkWith (fromLegacyAttr a) (T.pack s) . T.pack

image :: String -> String -> B.Inlines -> B.Inlines
image s = B.image (T.pack s) . T.pack

imageWith :: Attr -> String -> String -> B.Inlines -> B.Inlines
imageWith a s = B.imageWith (fromLegacyAttr a) (T.pack s) . T.pack

spanWith :: Attr -> B.Inlines -> B.Inlines
spanWith = B.spanWith . fromLegacyAttr

codeBlockWith :: Attr -> String -> B.Blocks
codeBlockWith a = B.codeBlockWith (fromLegacyAttr a) . T.pack

codeBlock :: String -> B.Blocks
codeBlock = B.codeBlock . T.pack

rawBlock :: String -> String -> B.Blocks
rawBlock s = B.rawBlock (T.pack s) . T.pack

headerWith :: Attr -> Int -> B.Inlines -> B.Blocks
headerWith = B.headerWith . fromLegacyAttr

divWith :: Attr -> B.Blocks -> B.Blocks
divWith = B.divWith . fromLegacyAttr
