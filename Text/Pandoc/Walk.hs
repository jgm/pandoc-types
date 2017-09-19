{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, CPP #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints -O2 #-}
#endif
#if MIN_VERSION_base(4,8,0)
#define OVERLAPS {-# OVERLAPPING #-}
#else
{-# LANGUAGE OverlappingInstances #-}
#define OVERLAPS
#endif
{-
Copyright (c) 2013-2017, John MacFarlane

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
   Module      : Text.Pandoc.Walk
   Copyright   : Copyright (C) 2013 John MacFarlane
   License     : BSD3

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
> extractURL (Link _ _ (u,_)) = [u]
> extractURL (Image _ _ (u,_)) = [u]
> extractURL _ = []
>
> extractURLs :: Pandoc -> [String]
> extractURLs = query extractURL
-}


module Text.Pandoc.Walk (Walkable(..))
where
import Control.Applicative (Applicative (pure), (<$>), (<*>))
import Control.Monad ((>=>))
import Data.Functor.Identity (Identity (runIdentity))
import Text.Pandoc.Definition
import qualified Data.Traversable as T
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import Data.Foldable (Foldable)
#if MIN_VERSION_base(4,8,0)
import Data.Monoid ((<>))
#else
import Data.Monoid
#endif

class Walkable a b where
  -- | @walk f x@ walks the structure @x@ (bottom up) and replaces every
  -- occurrence of an @a@ with the result of applying @f@ to it.
  walk  :: (a -> a) -> b -> b
  walk f = runIdentity . walkM (return . f)
  -- | A monadic version of 'walk'.
  walkM :: (Monad m, Applicative m, Functor m) => (a -> m a) -> b -> m b
  -- | @query f x@ walks the structure @x@ (bottom up) and applies @f@
  -- to every @a@, appending the results.
  query :: Monoid c => (a -> c) -> b -> c
  {-# MINIMAL walkM, query #-}

instance (Foldable t, Traversable t, Walkable a b) => Walkable a (t b) where
  walk f  = T.fmapDefault (walk f)
  walkM f = T.mapM (walkM f)
  query f = F.foldMap (query f)

instance OVERLAPS
        (Walkable a b, Walkable a c) => Walkable a (b,c) where
  walk f (x,y)  = (walk f x, walk f y)
  walkM f (x,y) = do x' <- walkM f x
                     y' <- walkM f y
                     return (x',y')
  query f (x,y) = mappend (query f x) (query f y)

instance Walkable Inline Inline where
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
  walkM f SoftBreak       = f SoftBreak
  walkM f LineBreak       = f LineBreak
  walkM f (Math mt s)     = f (Math mt s)
  walkM f (RawInline t s) = f $ RawInline t s
  walkM f (Link atr xs t) = Link atr <$> walkM f xs >>= f . ($ t)
  walkM f (Image atr xs t)= Image atr <$> walkM f xs >>= f . ($ t)
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
  query f SoftBreak       = f SoftBreak
  query f LineBreak       = f LineBreak
  query f (Math mt s)     = f (Math mt s)
  query f (RawInline t s) = f (RawInline t s)
  query f (Link atr xs t) = f (Link atr xs t) <> query f xs
  query f (Image atr xs t)= f (Image atr xs t) <> query f xs
  query f (Note bs)       = f (Note bs) <> query f bs
  query f (Span attr xs)  = f (Span attr xs) <> query f xs

instance OVERLAPS
         Walkable [Inline] [Inline] where
  walkM f = T.traverse walkInlineM >=> f
   where
    walkInlineM (Str xs)         = return (Str xs)
    walkInlineM (Emph xs)        = Emph <$> walkM f xs
    walkInlineM (Strong xs)      = Strong <$> walkM f xs
    walkInlineM (Strikeout xs)   = Strikeout <$> walkM f xs
    walkInlineM (Subscript xs)   = Subscript <$> walkM f xs
    walkInlineM (Superscript xs) = Superscript <$> walkM f xs
    walkInlineM (SmallCaps xs)   = SmallCaps <$> walkM f xs
    walkInlineM (Quoted qt xs)   = Quoted qt <$> walkM f xs
    walkInlineM (Link atr xs t)  = Link atr <$> walkM f xs <*> pure t
    walkInlineM (Image atr xs t) = Image atr <$> walkM f xs <*> pure t
    walkInlineM (Note bs)        = Note <$> walkM f bs
    walkInlineM (Span attr xs)   = Span attr <$> walkM f xs
    walkInlineM (Cite cs xs)     = Cite <$> walkM f cs <*> walkM f xs
    walkInlineM LineBreak        = return LineBreak
    walkInlineM SoftBreak        = return SoftBreak
    walkInlineM Space            = return Space
    walkInlineM x@Code {}        = return x
    walkInlineM x@Math {}        = return x
    walkInlineM x@RawInline {}   = return x

  query f inlns = f inlns <> mconcat (map queryInline inlns)
   where
    queryInline (Str _)         = mempty
    queryInline (Emph xs)       = query f xs
    queryInline (Strong xs)     = query f xs
    queryInline (Strikeout xs)  = query f xs
    queryInline (Subscript xs)  = query f xs
    queryInline (Superscript xs)= query f xs
    queryInline (SmallCaps xs)  = query f xs
    queryInline (Quoted _ xs)   = query f xs
    queryInline (Cite cs xs)    = query f cs <> query f xs
    queryInline (Code _ _)      = mempty
    queryInline Space           = mempty
    queryInline SoftBreak       = mempty
    queryInline LineBreak       = mempty
    queryInline (Math _ _)      = mempty
    queryInline (RawInline _ _) = mempty
    queryInline (Link _ xs _)   = query f xs
    queryInline (Image _ xs _)  = query f xs
    queryInline (Note bs)       = query f bs
    queryInline (Span _ xs)     = query f xs

instance Walkable Inline Block where
  walkM f (Para xs)                = Para <$> walkM f xs
  walkM f (Plain xs)               = Plain <$> walkM f xs
  walkM f (LineBlock xs)           = LineBlock <$> walkM f xs
  walkM _ (CodeBlock attr s)       = return $ CodeBlock attr s
  walkM _ (RawBlock t s)           = return $ RawBlock t s
  walkM f (BlockQuote bs)          = BlockQuote <$> walkM f bs
  walkM f (OrderedList a cs)       = OrderedList a <$> walkM f cs
  walkM f (BulletList cs)          = BulletList <$> walkM f cs
  walkM f (DefinitionList xs)      = DefinitionList <$> walkM f xs
  walkM f (Header lev attr xs)     = Header lev attr <$> walkM f xs
  walkM _ HorizontalRule           = return HorizontalRule
  walkM f (Table capt as ws hs rs) = do
                                     capt' <- walkM f capt
                                     hs' <- walkM f hs
                                     rs' <- walkM f rs
                                     return $ Table capt' as ws hs' rs'
  walkM f (Div attr bs)            = Div attr <$> walkM f bs
  walkM _ Null                     = return Null

  query f (Para xs)                = query f xs
  query f (Plain xs)               = query f xs
  query f (LineBlock xs)           = query f xs
  query _ (CodeBlock _ _)          = mempty
  query _ (RawBlock _ _)           = mempty
  query f (BlockQuote bs)          = query f bs
  query f (OrderedList _ cs)       = query f cs
  query f (BulletList cs)          = query f cs
  query f (DefinitionList xs)      = query f xs
  query f (Header _ _ xs)          = query f xs
  query _ HorizontalRule           = mempty
  query f (Table capt _ _ hs rs)   = query f capt <> query f hs <> query f rs
  query f (Div _ bs)               = query f bs
  query _ Null                     = mempty

instance Walkable [Inline] Block where
  walkM f (Para xs)                = Para <$> walkM f xs
  walkM f (Plain xs)               = Plain <$> walkM f xs
  walkM f (LineBlock xs)           = LineBlock <$> mapM (walkM f) xs
  walkM f (BlockQuote bs)          = BlockQuote <$> walkM f bs
  walkM f (OrderedList a cs)       = OrderedList a <$> walkM f cs
  walkM f (BulletList cs)          = BulletList <$> walkM f cs
  walkM f (DefinitionList xs)      = DefinitionList <$> walkM f xs
  walkM f (Header lev attr xs)     = Header lev attr <$> walkM f xs
  walkM f (Table capt as ws hs rs) = Table <$> walkM f capt <*> pure as
                                           <*> pure ws <*> walkM f hs
                                           <*> walkM f rs
  walkM f (Div attr bs)            = Div attr <$> walkM f bs
  walkM _ x                        = return x

  query f (Para xs)                = query f xs
  query f (Plain xs)               = query f xs
  query f (LineBlock xs)           = query f xs
  query _ CodeBlock {}             = mempty
  query _ RawBlock {}              = mempty
  query f (BlockQuote bs)          = query f bs
  query f (OrderedList _ cs)       = query f cs
  query f (BulletList cs)          = query f cs
  query f (DefinitionList xs)      = query f xs
  query f (Header _ _ xs)          = query f xs
  query _ HorizontalRule           = mempty
  query f (Table capt _ _ hs rs)   = query f capt <> query f hs <> query f rs
  query f (Div _ bs)               = query f bs
  query _ Null                     = mempty

instance Walkable Block Block where
  walkM f (Para xs)                = Para <$> walkM f xs >>= f
  walkM f (Plain xs)               = Plain <$> walkM f xs >>= f
  walkM f (LineBlock xs)           = LineBlock <$> walkM f xs >>= f
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
  query f (LineBlock xs)           = f (LineBlock xs) <> query f xs
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

instance OVERLAPS
         Walkable [Block] [Block] where
  walkM f = T.traverse walkBlockM >=> f
   where
    walkBlockM (Para xs)                = Para <$> walkM f xs
    walkBlockM (Plain xs)               = Plain <$> walkM f xs
    walkBlockM (LineBlock xs)           = LineBlock <$> walkM f xs
    walkBlockM (BlockQuote xs)          = BlockQuote <$> walkM f xs
    walkBlockM (OrderedList a cs)       = OrderedList a <$> walkM f cs
    walkBlockM (BulletList cs)          = BulletList <$> walkM f cs
    walkBlockM (DefinitionList xs)      = DefinitionList <$> walkM f xs
    walkBlockM (Header lev attr xs)     = Header lev attr <$> walkM f xs
    walkBlockM (Div attr bs')           = Div attr <$> walkM f bs'
    walkBlockM x@CodeBlock {}           = return x
    walkBlockM x@RawBlock {}            = return x
    walkBlockM HorizontalRule           = return HorizontalRule
    walkBlockM Null                     = return Null
    walkBlockM (Table capt as ws hs rs) = do capt' <- walkM f capt
                                             hs' <- walkM f hs
                                             rs' <- walkM f rs
                                             return $ Table capt' as ws hs' rs'

  query f blks = f blks <> mconcat (map queryBlocks blks)
   where
     queryBlocks (Para xs)              = query f xs
     queryBlocks (Plain xs)             = query f xs
     queryBlocks (LineBlock xs)         = query f xs
     queryBlocks CodeBlock {}           = mempty
     queryBlocks RawBlock {}            = mempty
     queryBlocks (BlockQuote bs)        = query f bs
     queryBlocks (OrderedList _ cs)     = query f cs
     queryBlocks (BulletList cs)        = query f cs
     queryBlocks (DefinitionList xs)    = query f xs
     queryBlocks (Header _ _ xs)        = query f xs
     queryBlocks HorizontalRule         = mempty
     queryBlocks (Table capt _ _ hs rs) = query f capt <> query f hs <> query f rs
     queryBlocks (Div _ bs)             = query f bs
     queryBlocks Null                   = mempty

instance Walkable Block Inline where
  walkM _ (Str xs)        = return $ Str xs
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
  walkM _ (Code attr s)   = return $ Code attr s
  walkM _ Space           = return $ Space
  walkM _ SoftBreak       = return $ SoftBreak
  walkM _ LineBreak       = return $ LineBreak
  walkM _ (Math mt s)     = return $ Math mt s
  walkM _ (RawInline t s) = return $ RawInline t s
  walkM f (Link atr xs t) = (\lab -> Link atr lab t) <$> walkM f xs
  walkM f (Image atr xs t)= (\lab -> Image atr lab t) <$> walkM f xs
  walkM f (Note bs)       = Note <$> walkM f bs
  walkM f (Span attr xs)  = Span attr <$> walkM f xs

  query _ (Str _)         = mempty
  query f (Emph xs)       = query f xs
  query f (Strong xs)     = query f xs
  query f (Strikeout xs)  = query f xs
  query f (Subscript xs)  = query f xs
  query f (Superscript xs)= query f xs
  query f (SmallCaps xs)  = query f xs
  query f (Quoted _ xs)   = query f xs
  query f (Cite cs xs)    = query f cs <> query f xs
  query _ (Code _ _)      = mempty
  query _ Space           = mempty
  query _ SoftBreak       = mempty
  query _ LineBreak       = mempty
  query _ (Math _ _)      = mempty
  query _ (RawInline _ _) = mempty
  query f (Link _ xs _)   = query f xs
  query f (Image _ xs _)  = query f xs
  query f (Note bs)       = query f bs
  query f (Span _ xs)     = query f xs

instance Walkable [Block] Inline where
  walkM _ (Str xs)        = return $ Str xs
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
  walkM _ (Code attr s)   = return $ Code attr s
  walkM _ Space           = return Space
  walkM _ SoftBreak       = return SoftBreak
  walkM _ LineBreak       = return LineBreak
  walkM _ (Math mt s)     = return $ Math mt s
  walkM _ (RawInline t s) = return $ RawInline t s
  walkM f (Link atr xs t) = (\lab -> Link atr lab t) <$> walkM f xs
  walkM f (Image atr xs t)= (\lab -> Image atr lab t) <$> walkM f xs
  walkM f (Note bs)       = Note <$> walkM f bs
  walkM f (Span attr xs)  = Span attr <$> walkM f xs

  query _ (Str _)         = mempty
  query f (Emph xs)       = query f xs
  query f (Strong xs)     = query f xs
  query f (Strikeout xs)  = query f xs
  query f (Subscript xs)  = query f xs
  query f (Superscript xs)= query f xs
  query f (SmallCaps xs)  = query f xs
  query f (Quoted _ xs)   = query f xs
  query f (Cite cs xs)    = query f cs <> query f xs
  query _ (Code _ _)      = mempty
  query _ Space           = mempty
  query _ SoftBreak       = mempty
  query _ LineBreak       = mempty
  query _ (Math _ _)      = mempty
  query _ (RawInline _ _) = mempty
  query f (Link _ xs _)   = query f xs
  query f (Image _ xs _)  = query f xs
  query f (Note bs)       = query f bs
  query f (Span _ xs)     = query f xs

instance Walkable Block Pandoc where
  walk f (Pandoc m bs)  = Pandoc (walk f m) (walk f bs)
  walkM f (Pandoc m bs) = do m' <- walkM f m
                             bs' <- walkM f bs
                             return $ Pandoc m' bs'
  query f (Pandoc m bs) = query f m <> query f bs

instance Walkable [Block] Pandoc where
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

instance Walkable [Inline] Pandoc where
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

instance Walkable [Inline] Meta where
  walk f (Meta metamap) = Meta $ walk f metamap
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance Walkable Block Meta where
  walk f (Meta metamap)  = Meta $ walk f metamap
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance Walkable [Block] Meta where
  walk f (Meta metamap)  = Meta $ walk f metamap
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance Walkable Inline MetaValue where
  walkM f (MetaList xs)    = MetaList <$> walkM f xs
  walkM _ (MetaBool b)     = return $ MetaBool b
  walkM _ (MetaString s)   = return $ MetaString s
  walkM f (MetaInlines xs) = MetaInlines <$> walkM f xs
  walkM f (MetaBlocks bs)  = MetaBlocks <$> walkM f bs
  walkM f (MetaMap m)      = MetaMap <$> walkM f m

  query f (MetaList xs)    = query f xs
  query _ (MetaBool _)     = mempty
  query _ (MetaString _)   = mempty
  query f (MetaInlines xs) = query f xs
  query f (MetaBlocks bs)  = query f bs
  query f (MetaMap m)      = query f m

instance Walkable [Inline] MetaValue where
  walkM f (MetaList xs)    = MetaList <$> walkM f xs
  walkM _ (MetaBool b)     = return $ MetaBool b
  walkM _ (MetaString s)   = return $ MetaString s
  walkM f (MetaInlines xs) = MetaInlines <$> walkM f xs
  walkM f (MetaBlocks bs)  = MetaBlocks <$> walkM f bs
  walkM f (MetaMap m)      = MetaMap <$> walkM f m

  query f (MetaList xs)    = query f xs
  query _ (MetaBool _)     = mempty
  query _ (MetaString _)   = mempty
  query f (MetaInlines xs) = query f xs
  query f (MetaBlocks bs)  = query f bs
  query f (MetaMap m)      = query f m

instance Walkable Block MetaValue where
  walkM f (MetaList xs)    = MetaList <$> walkM f xs
  walkM _ (MetaBool b)     = return $ MetaBool b
  walkM _ (MetaString s)   = return $ MetaString s
  walkM f (MetaInlines xs) = MetaInlines <$> walkM f xs
  walkM f (MetaBlocks bs)  = MetaBlocks <$> walkM f bs
  walkM f (MetaMap m)      = MetaMap <$> walkM f m

  query f (MetaList xs)    = query f xs
  query _ (MetaBool _)     = mempty
  query _ (MetaString _)   = mempty
  query f (MetaInlines xs) = query f xs
  query f (MetaBlocks bs)  = query f bs
  query f (MetaMap m)      = query f m

instance Walkable [Block] MetaValue where
  walkM f (MetaList xs)    = MetaList <$> walkM f xs
  walkM _ (MetaBool b)     = return $ MetaBool b
  walkM _ (MetaString s)   = return $ MetaString s
  walkM f (MetaInlines xs) = MetaInlines <$> walkM f xs
  walkM f (MetaBlocks bs)  = MetaBlocks <$> walkM f bs
  walkM f (MetaMap m)      = MetaMap <$> walkM f m

  query f (MetaList xs)    = query f xs
  query _ (MetaBool _)     = mempty
  query _ (MetaString _)   = mempty
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
  query f (Citation _ pref suff _ _ _) =
    query f pref <> query f suff

instance Walkable [Inline] Citation where
  walk f (Citation id' pref suff mode notenum hash) =
    Citation id' (walk f pref) (walk f suff) mode notenum hash
  walkM f (Citation id' pref suff mode notenum hash) =
    do pref' <- walkM f pref
       suff' <- walkM f suff
       return $ Citation id' pref' suff' mode notenum hash
  query f (Citation _ pref suff _ _ _) =
    query f pref <> query f suff


instance Walkable Block Citation where
  walk f (Citation id' pref suff mode notenum hash) =
    Citation id' (walk f pref) (walk f suff) mode notenum hash
  walkM f (Citation id' pref suff mode notenum hash) =
    do pref' <- walkM f pref
       suff' <- walkM f suff
       return $ Citation id' pref' suff' mode notenum hash
  query f (Citation _ pref suff _ _ _) =
    query f pref <> query f suff

instance Walkable [Block] Citation where
  walk f (Citation id' pref suff mode notenum hash) =
    Citation id' (walk f pref) (walk f suff) mode notenum hash
  walkM f (Citation id' pref suff mode notenum hash) =
    do pref' <- walkM f pref
       suff' <- walkM f suff
       return $ Citation id' pref' suff' mode notenum hash
  query f (Citation _ pref suff _ _ _) =
    query f pref <> query f suff
