{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables,
    OverlappingInstances #-}
{-
Copyright (C) 2013 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Walk
   Copyright   : Copyright (C) 2013 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for manipulating 'Pandoc' documents or extracting
information from them by walking the 'Pandoc' structure (or
intermediate structures like '[Block]' or '[Inline]'.
These are faster (by a factor of four or five) than the generic
functions defined in @Text.Pandoc.Generic@.

Here's a simple example, defining a function that replaces all the level 3+
headers in a document with regular paragraphs in ALL CAPS:

> import Text.Pandoc.Definition
> import Text.Pandoc.Walk
> import Data.Char (toUpper)
>
> modHeader :: Block -> Block
> modHeader (Header n _ xs) | n >= 3 = Para $ walk allCaps xs
> modHeader x = x
>
> allCaps :: Inline -> Inline
> allCaps (Str xs) = Str $ map toUpper xs
> allCaps x = x
>
> changeHeaders :: Pandoc -> Pandoc
> changeHeaders = walk modHeader

'query' can be used, for example, to compile a list of URLs
linked to in a document:

> extractURL :: Inline -> [String]
> extractURL (Link _ (u,_)) = [u]
> extractURL (Image _ (u,_)) = [u]
> extractURL _ = []
>
> extractURLs :: Pandoc -> [String]
> extractURLs = query extractURL
-}


module Text.Pandoc.Walk (Walkable(..))
where
import Control.Applicative ((<$>), (<*>))
import Text.Pandoc.Definition
import Text.Pandoc.Builder ((<>))
import qualified Data.Traversable as T
import Data.Traversable (Traversable, traverse)
import qualified Data.Foldable as F
import Data.Foldable (Foldable, foldMap)
import qualified Data.Map as M
import Data.Monoid


class Walkable a b where
  -- | @walk f x@ walks the structure @x@ (bottom up) and replaces every
  -- occurrence of an @a@ with the result of applying @f@ to it.
  walk  :: (a -> a) -> b -> b
  -- | A monadic version of 'walk'.
  walkM :: (Monad m, Functor m) => (a -> m a) -> b -> m b
  -- | @query f x@ walks the structure @x@ (bottom up) and applies @f@
  -- to every @a@, appending the results.
  query :: Monoid c => (a -> c) -> b -> c

instance (Foldable t, Traversable t, Walkable a b) => Walkable a (t b) where
  walk f  = T.fmapDefault (walk f)
  walkM f = T.mapM (walkM f)
  query f = F.foldMap (query f)

instance (Walkable a b, Walkable a c) => Walkable a (b,c) where
  walk f (x,y)  = (walk f x, walk f y)
  walkM f (x,y) = do x' <- walkM f x
                     y' <- walkM f y
                     return (x',y')
  query f (x,y) = mappend (query f x) (query f y)

instance Walkable Inline Inline where
  walk f (Str xs)         = f $ Str xs
  walk f (Emph xs)        = f $ Emph (walk f xs)
  walk f (Strong xs)      = f $ Strong (walk f xs)
  walk f (Strikeout xs)   = f $ Strikeout (walk f xs)
  walk f (Subscript xs)   = f $ Subscript (walk f xs)
  walk f (Superscript xs) = f $ Superscript (walk f xs)
  walk f (SmallCaps xs)   = f $ SmallCaps (walk f xs)
  walk f (Quoted qt xs)   = f $ Quoted qt (walk f xs)
  walk f (Cite cs xs)     = f $ Cite (walk f cs) (walk f xs)
  walk f (Code attr s)    = f $ Code attr s
  walk f Space            = f Space
  walk f LineBreak        = f LineBreak
  walk f (Math mt s)      = f (Math mt s)
  walk f (RawInline t s)  = f $ RawInline t s
  walk f (Link xs t)      = f $ Link (walk f xs) t
  walk f (Image xs t)     = f $ Image (walk f xs) t
  walk f (Note bs)        = f $ Note (walk f bs)
  walk f (Span attr xs)   = f $ Span attr (walk f xs)

  walkM f (Str xs)        = f $ Str xs
  walkM f (Emph xs)       = Emph <$> walkM f xs >>= f
  walkM f (Strong xs)     = Strong <$> walkM f xs >>= f
  walkM f (Strikeout xs)  = Strikeout <$> walkM f xs >>= f
  walkM f (Subscript xs)  = Subscript <$> walkM f xs >>= f
  walkM f (Superscript xs)= Superscript <$> walkM f xs >>= f
  walkM f (SmallCaps xs)  = SmallCaps <$> walkM f xs >>= f
  walkM f (Quoted qt xs)  = Quoted qt <$> walkM f xs >>= f
  walkM f (Cite cs xs)    = do cs' <- walkM f cs
                               xs' <- walkM f xs
                               f $ Cite cs' xs'
  walkM f (Code attr s)   = f $ Code attr s
  walkM f Space           = f Space
  walkM f LineBreak       = f LineBreak
  walkM f (Math mt s)     = f (Math mt s)
  walkM f (RawInline t s) = f $ RawInline t s
  walkM f (Link xs t)     = Link <$> walkM f xs >>= f . ($ t)
  walkM f (Image xs t)    = Image <$> walkM f xs >>= f . ($ t)
  walkM f (Note bs)       = Note <$> walkM f bs >>= f
  walkM f (Span attr xs)  = Span attr <$> walkM f xs >>= f

  query f (Str xs)        = f (Str xs)
  query f (Emph xs)       = f (Emph xs) <> query f xs
  query f (Strong xs)     = f (Strong xs) <> query f xs
  query f (Strikeout xs)  = f (Strikeout xs) <> query f xs
  query f (Subscript xs)  = f (Subscript xs) <> query f xs
  query f (Superscript xs)= f (Superscript xs) <> query f xs
  query f (SmallCaps xs)  = f (SmallCaps xs) <> query f xs
  query f (Quoted qt xs)  = f (Quoted qt xs) <> query f xs
  query f (Cite cs xs)    = f (Cite cs xs) <> query f cs <> query f xs
  query f (Code attr s)   = f (Code attr s)
  query f Space           = f Space
  query f LineBreak       = f LineBreak
  query f (Math mt s)     = f (Math mt s)
  query f (RawInline t s) = f (RawInline t s)
  query f (Link xs t)     = f (Link xs t) <> query f xs
  query f (Image xs t)    = f (Image xs t) <> query f xs
  query f (Note bs)       = f (Note bs) <> query f bs
  query f (Span attr xs)  = f (Span attr xs) <> query f xs

instance Walkable Inline Block where
  walk f (Para xs)                = Para $ walk f xs
  walk f (Plain xs)               = Plain $ walk f xs
  walk f (CodeBlock attr s)       = CodeBlock attr s
  walk f (RawBlock t s)           = RawBlock t s
  walk f (BlockQuote bs)          = BlockQuote $ walk f bs
  walk f (OrderedList a cs)       = OrderedList a $ walk f cs
  walk f (BulletList cs)          = BulletList $ walk f cs
  walk f (DefinitionList xs)      = DefinitionList $ walk f xs
  walk f (Header lev attr xs)     = Header lev attr $ walk f xs
  walk f HorizontalRule           = HorizontalRule
  walk f (Table capt as ws hs rs) = Table (walk f capt) as ws (walk f hs) (walk f rs)
  walk f (Div attr bs)            = Div attr (walk f bs)
  walk f Null                     = Null

  walkM f (Para xs)                = Para <$> walkM f xs
  walkM f (Plain xs)               = Plain <$> walkM f xs
  walkM f (CodeBlock attr s)       = return $ CodeBlock attr s
  walkM f (RawBlock t s)           = return $ RawBlock t s
  walkM f (BlockQuote bs)          = BlockQuote <$> walkM f bs
  walkM f (OrderedList a cs)       = OrderedList a <$> walkM f cs
  walkM f (BulletList cs)          = BulletList <$> walkM f cs
  walkM f (DefinitionList xs)      = DefinitionList <$> walkM f xs
  walkM f (Header lev attr xs)     = Header lev attr <$> walkM f xs
  walkM f HorizontalRule           = return HorizontalRule
  walkM f (Table capt as ws hs rs) = do
                                     capt' <- walkM f capt
                                     hs' <- walkM f hs
                                     rs' <- walkM f rs
                                     return $ Table capt' as ws hs' rs'
  walkM f (Div attr bs)            = Div attr <$> (walkM f bs)
  walkM f Null                     = return Null

  query f (Para xs)                = query f xs
  query f (Plain xs)               = query f xs
  query f (CodeBlock attr s)       = mempty
  query f (RawBlock t s)           = mempty
  query f (BlockQuote bs)          = query f bs
  query f (OrderedList a cs)       = query f cs
  query f (BulletList cs)          = query f cs
  query f (DefinitionList xs)      = query f xs
  query f (Header lev attr xs)     = query f xs
  query f HorizontalRule           = mempty
  query f (Table capt as ws hs rs) = query f capt <> query f hs <> query f rs
  query f (Div attr bs)            = query f bs
  query f Null                     = mempty

instance Walkable Block Block where
  walk f (Para xs)                = f $ Para $ walk f xs
  walk f (Plain xs)               = f $ Plain $ walk f xs
  walk f (CodeBlock attr s)       = f $ CodeBlock attr s
  walk f (RawBlock t s)           = f $ RawBlock t s
  walk f (BlockQuote bs)          = f $ BlockQuote $ walk f bs
  walk f (OrderedList a cs)       = f $ OrderedList a $ walk f cs
  walk f (BulletList cs)          = f $ BulletList $ walk f cs
  walk f (DefinitionList xs)      = f $ DefinitionList $ walk f xs
  walk f (Header lev attr xs)     = f $ Header lev attr $ walk f xs
  walk f HorizontalRule           = f $ HorizontalRule
  walk f (Table capt as ws hs rs) = f $ Table (walk f capt) as ws (walk f hs)
                                                     (walk f rs)
  walk f (Div attr bs)            = f $ Div attr (walk f bs)
  walk f Null                     = Null

  walkM f (Para xs)                = Para <$> walkM f xs >>= f
  walkM f (Plain xs)               = Plain <$> walkM f xs >>= f
  walkM f (CodeBlock attr s)       = f $ CodeBlock attr s
  walkM f (RawBlock t s)           = f $ RawBlock t s
  walkM f (BlockQuote bs)          = BlockQuote <$> walkM f bs >>= f
  walkM f (OrderedList a cs)       = OrderedList a <$> walkM f cs >>= f
  walkM f (BulletList cs)          = BulletList <$> walkM f cs >>= f
  walkM f (DefinitionList xs)      = DefinitionList <$> walkM f xs >>= f
  walkM f (Header lev attr xs)     = Header lev attr <$> walkM f xs >>= f
  walkM f HorizontalRule           = f $ HorizontalRule
  walkM f (Table capt as ws hs rs) = do capt' <- walkM f capt
                                        hs' <- walkM f hs
                                        rs' <- walkM f rs
                                        f $ Table capt' as ws hs' rs'
  walkM f (Div attr bs)            = Div attr <$> walkM f bs >>= f
  walkM f Null                     = f Null

  query f (Para xs)                = f (Para xs) <> query f xs
  query f (Plain xs)               = f (Plain xs) <> query f xs
  query f (CodeBlock attr s)       = f $ CodeBlock attr s
  query f (RawBlock t s)           = f $ RawBlock t s
  query f (BlockQuote bs)          = f (BlockQuote bs) <> query f bs
  query f (OrderedList a cs)       = f (OrderedList a cs) <> query f cs
  query f (BulletList cs)          = f (BulletList cs) <> query f cs
  query f (DefinitionList xs)      = f (DefinitionList xs) <> query f xs
  query f (Header lev attr xs)     = f (Header lev attr xs) <> query f xs
  query f HorizontalRule           = f $ HorizontalRule
  query f (Table capt as ws hs rs) = f (Table capt as ws hs rs) <>
                                       query f capt <> query f hs <> query f rs
  query f (Div attr bs)            = f (Div attr bs) <> query f bs
  query f Null                     = f Null

instance Walkable Block Inline where
  walk f (Str xs)        = Str xs
  walk f (Emph xs)       = Emph (walk f xs)
  walk f (Strong xs)     = Strong (walk f xs)
  walk f (Strikeout xs)  = Strikeout (walk f xs)
  walk f (Subscript xs)  = Subscript (walk f xs)
  walk f (Superscript xs)= Superscript (walk f xs)
  walk f (SmallCaps xs)  = SmallCaps (walk f xs)
  walk f (Quoted qt xs)  = Quoted qt (walk f xs)
  walk f (Cite cs xs)    = Cite (walk f cs) (walk f xs)
  walk f (Code attr s)   = Code attr s
  walk f Space           = Space
  walk f LineBreak       = LineBreak
  walk f (Math mt s)     = Math mt s
  walk f (RawInline t s) = RawInline t s
  walk f (Link xs t)     = Link (walk f xs) t
  walk f (Image xs t)    = Image (walk f xs) t
  walk f (Note bs)       = Note (walk f bs)
  walk f (Span attr xs)  = Span attr (walk f xs)

  walkM f (Str xs)        = return $ Str xs
  walkM f (Emph xs)       = Emph <$> walkM f xs
  walkM f (Strong xs)     = Strong <$> walkM f xs
  walkM f (Strikeout xs)  = Strikeout <$> walkM f xs
  walkM f (Subscript xs)  = Subscript <$> walkM f xs
  walkM f (Superscript xs)= Superscript <$> walkM f xs
  walkM f (SmallCaps xs)  = SmallCaps <$> walkM f xs
  walkM f (Quoted qt xs)  = Quoted qt <$> walkM f xs
  walkM f (Cite cs xs)    = do cs' <- walkM f cs
                               xs' <- walkM f xs
                               return $ Cite cs' xs'
  walkM f (Code attr s)   = return $ Code attr s
  walkM f Space           = return $ Space
  walkM f LineBreak       = return $ LineBreak
  walkM f (Math mt s)     = return $ Math mt s
  walkM f (RawInline t s) = return $ RawInline t s
  walkM f (Link xs t)     = (\lab -> Link lab t) <$> walkM f xs
  walkM f (Image xs t)    = (\lab -> Image lab t) <$> walkM f xs
  walkM f (Note bs)       = Note <$> walkM f bs
  walkM f (Span attr xs)  = Span attr <$> walkM f xs

  query f (Str xs)        = mempty
  query f (Emph xs)       = query f xs
  query f (Strong xs)     = query f xs
  query f (Strikeout xs)  = query f xs
  query f (Subscript xs)  = query f xs
  query f (Superscript xs)= query f xs
  query f (SmallCaps xs)  = query f xs
  query f (Quoted qt xs)  = query f xs
  query f (Cite cs xs)    = query f cs <> query f xs
  query f (Code attr s)   = mempty
  query f Space           = mempty
  query f LineBreak       = mempty
  query f (Math mt s)     = mempty
  query f (RawInline t s) = mempty
  query f (Link xs t)     = query f xs
  query f (Image xs t)    = query f xs
  query f (Note bs)       = query f bs
  query f (Span attr xs)  = query f xs

instance Walkable Block Pandoc where
  walk f (Pandoc m bs)  = Pandoc (walk f m) (walk f bs)
  walkM f (Pandoc m bs) = do m' <- walkM f m
                             bs' <- walkM f bs
                             return $ Pandoc m' bs'
  query f (Pandoc m bs) = query f m <> query f bs

instance Walkable Inline Pandoc where
  walk f (Pandoc m bs)  = Pandoc (walk f m) (walk f bs)
  walkM f (Pandoc m bs) = do m' <- walkM f m
                             bs' <- walkM f bs
                             return $ Pandoc m' bs'
  query f (Pandoc m bs) = query f m <> query f bs

instance Walkable Pandoc Pandoc where
  walk f = f
  walkM f = f
  query f = f

instance Walkable Meta Meta where
  walk f = f
  walkM f = f
  query f = f

instance Walkable Inline Meta where
  walk f (Meta metamap)  = Meta $ walk f metamap
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance Walkable Block Meta where
  walk f (Meta metamap)  = Meta $ walk f metamap
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance Walkable Inline MetaValue where
  walk f (MetaList xs)    = MetaList $ walk f xs
  walk f (MetaBool b)     = MetaBool b
  walk f (MetaString s)   = MetaString s
  walk f (MetaInlines xs) = MetaInlines $ walk f xs
  walk f (MetaBlocks bs)  = MetaBlocks $ walk f bs
  walk f (MetaMap m)      = MetaMap $ walk f m

  walkM f (MetaList xs)    = MetaList <$> walkM f xs
  walkM f (MetaBool b)     = return $ MetaBool b
  walkM f (MetaString s)   = return $ MetaString s
  walkM f (MetaInlines xs) = MetaInlines <$> walkM f xs
  walkM f (MetaBlocks bs)  = MetaBlocks <$> walkM f bs
  walkM f (MetaMap m)      = MetaMap <$> walkM f m

  query f (MetaList xs)    = query f xs
  query f (MetaBool b)     = mempty
  query f (MetaString s)   = mempty
  query f (MetaInlines xs) = query f xs
  query f (MetaBlocks bs)  = query f bs
  query f (MetaMap m)      = query f m

instance Walkable Block MetaValue where
  walk f (MetaList xs)    = MetaList $ walk f xs
  walk f (MetaBool b)     = MetaBool b
  walk f (MetaString s)   = MetaString s
  walk f (MetaInlines xs) = MetaInlines $ walk f xs
  walk f (MetaBlocks bs)  = MetaBlocks $ walk f bs
  walk f (MetaMap m)      = MetaMap $ walk f m

  walkM f (MetaList xs)    = MetaList <$> walkM f xs
  walkM f (MetaBool b)     = return $ MetaBool b
  walkM f (MetaString s)   = return $ MetaString s
  walkM f (MetaInlines xs) = MetaInlines <$> walkM f xs
  walkM f (MetaBlocks bs)  = MetaBlocks <$> walkM f bs
  walkM f (MetaMap m)      = MetaMap <$> walkM f m

  query f (MetaList xs)    = query f xs
  query f (MetaBool b)     = mempty
  query f (MetaString s)   = mempty
  query f (MetaInlines xs) = query f xs
  query f (MetaBlocks bs)  = query f bs
  query f (MetaMap m)      = query f m

instance Walkable Inline Citation where
  walk f (Citation id' pref suff mode notenum hash) =
    Citation id' (walk f pref) (walk f suff) mode notenum hash
  walkM f (Citation id' pref suff mode notenum hash) =
    do pref' <- walkM f pref
       suff' <- walkM f suff
       return $ Citation id' pref' suff' mode notenum hash
  query f (Citation id' pref suff mode notenum hash) =
    query f pref <> query f suff

instance Walkable Block Citation where
  walk f (Citation id' pref suff mode notenum hash) =
    Citation id' (walk f pref) (walk f suff) mode notenum hash
  walkM f (Citation id' pref suff mode notenum hash) =
    do pref' <- walkM f pref
       suff' <- walkM f suff
       return $ Citation id' pref' suff' mode notenum hash
  query f (Citation id' pref suff mode notenum hash) =
    query f pref <> query f suff

instance Walkable a b => Walkable a [b] where
  walk f xs  = map (walk f) xs
  walkM f xs = mapM (walkM f) xs
  query f xs = mconcat $ map (query f) xs
