{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints -O2 #-}
#endif
#if MIN_VERSION_base(4,8,0)
#define OVERLAPS {-# OVERLAPPING #-}
#define OVERLAPPABLE_ {-# OVERLAPPABLE #-}
#else
{-# LANGUAGE OverlappingInstances #-}
#define OVERLAPS
#define OVERLAPPABLE_
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
intermediate structures like '[Block' string]' or '[Inline]'.
These are faster (by a factor of four or five) than the generic
functions defined in @Text.Pandoc.Generic@.

Here's a simple example, defining a function that replaces all the level 3+
headers in a document with regular paragraphs in ALL CAPS:

> import Text.Pandoc.Definition
> import Text.Pandoc.Walk
> import Data.Char (toUpper)
>
> modHeader :: (Block' string) -> (Block' string)
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

instance OVERLAPPABLE_
         (Foldable t, Traversable t, Walkable a b)
         => Walkable a (t b) where
  walk f  = T.fmapDefault (walk f)
  walkM f = T.mapM (walkM f)
  query f = F.foldMap (query f)

instance OVERLAPS
        (Walkable a b, Walkable a c) => Walkable a (b, c) where
  walk f (x,y)  = (walk f x, walk f y)
  walkM f (x,y) = do x' <- walkM f x
                     y' <- walkM f y
                     return (x',y')
  query f (x,y) = mappend (query f x) (query f y)

instance OVERLAPS
         Walkable (Inline' string) (Inline' string) where
  walkM f x = walkInlineM f x >>= f
  query f x = f x <> queryInline f x

instance OVERLAPS
         Walkable [Inline' string] [Inline' string] where
  walkM f = T.traverse (walkInlineM f) >=> f
  query f inlns = f inlns <> mconcat (map (queryInline f) inlns)

instance OVERLAPS
         Walkable (Inline' string) (Block' string) where
  walkM f x = walkBlockM f x
  query f x = queryBlock f x

instance OVERLAPS
         Walkable [Inline' string] (Block' string) where
  walkM f x = walkBlockM f x
  query f x = queryBlock f x

instance OVERLAPS
         Walkable (Block' string) (Block' string) where
  walkM f x = walkBlockM f x >>= f
  query f x = f x <> queryBlock f x

instance OVERLAPS
         Walkable [Block' string] [Block' string] where
  walkM f = T.traverse (walkBlockM f) >=> f
  query f blks = f blks <> mconcat (map (queryBlock f) blks)

instance OVERLAPS
         Walkable (Block' string) (Inline' string) where
  walkM f x = walkInlineM f x
  query f x = queryInline f x

instance OVERLAPS
         Walkable [Block' string] (Inline' string) where
  walkM f x = walkInlineM f x
  query f x = queryInline f x

instance OVERLAPS
         Walkable (Block' string) (Pandoc' string) where
  walkM = walkPandocM
  query = queryPandoc

instance OVERLAPS
         Walkable [Block' string] (Pandoc' string) where
  walkM = walkPandocM
  query = queryPandoc

instance OVERLAPS
         Walkable (Inline' string) (Pandoc' string) where
  walkM = walkPandocM
  query = queryPandoc

instance OVERLAPS
         Walkable [Inline' string] (Pandoc' string) where
  walkM = walkPandocM
  query = queryPandoc

instance OVERLAPS
         Walkable (Pandoc' string) (Pandoc' string) where
  walkM f = f
  query f = f

instance OVERLAPS
         Walkable (Meta' string) (Meta' string) where
  walkM f = f
  query f = f

instance OVERLAPS
         Walkable (Inline' string) (Meta' string) where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance OVERLAPS
         Walkable [Inline' string] (Meta' string) where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance OVERLAPS
         Walkable (Block' string) (Meta' string) where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance OVERLAPS
         Walkable [Block' string] (Meta' string) where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance OVERLAPS
         Walkable (Inline' string) (MetaValue' string) where
  walkM = walkMetaValueM
  query = queryMetaValue

instance OVERLAPS
         Walkable [Inline' string] (MetaValue' string) where
  walkM = walkMetaValueM
  query = queryMetaValue

instance OVERLAPS
         Walkable (Block' string) (MetaValue' string) where
  walkM = walkMetaValueM
  query = queryMetaValue

instance OVERLAPS
         Walkable [Block' string] (MetaValue' string) where
  walkM = walkMetaValueM
  query = queryMetaValue

instance OVERLAPS
         Walkable (Inline' string) (Citation' string) where
  walkM = walkCitationM
  query = queryCitation

instance OVERLAPS
         Walkable [Inline' string] (Citation' string) where
  walkM = walkCitationM
  query = queryCitation

instance OVERLAPS
         Walkable (Block' string) (Citation' string) where
  walkM = walkCitationM
  query = queryCitation

instance OVERLAPS
         Walkable [Block' string] (Citation' string) where
  walkM = walkCitationM
  query = queryCitation

walkInlineM :: (Walkable a (Citation' string),
                Walkable a [Block' string],
                Walkable a [Inline' string],
                Monad m, Applicative m, Functor m)
            => (a -> m a) -> (Inline' string) -> m (Inline' string)
walkInlineM _ (Str xs)         = return (Str xs)
walkInlineM f (Emph xs)        = Emph <$> walkM f xs
walkInlineM f (Strong xs)      = Strong <$> walkM f xs
walkInlineM f (Strikeout xs)   = Strikeout <$> walkM f xs
walkInlineM f (Subscript xs)   = Subscript <$> walkM f xs
walkInlineM f (Superscript xs) = Superscript <$> walkM f xs
walkInlineM f (SmallCaps xs)   = SmallCaps <$> walkM f xs
walkInlineM f (Quoted qt xs)   = Quoted qt <$> walkM f xs
walkInlineM f (Link atr xs t)  = Link atr <$> walkM f xs <*> pure t
walkInlineM f (Image atr xs t) = Image atr <$> walkM f xs <*> pure t
walkInlineM f (Note bs)        = Note <$> walkM f bs
walkInlineM f (Span attr xs)   = Span attr <$> walkM f xs
walkInlineM f (Cite cs xs)     = Cite <$> walkM f cs <*> walkM f xs
walkInlineM _ LineBreak        = return LineBreak
walkInlineM _ SoftBreak        = return SoftBreak
walkInlineM _ Space            = return Space
walkInlineM _ x@Code {}        = return x
walkInlineM _ x@Math {}        = return x
walkInlineM _ x@RawInline {}   = return x

walkBlockM :: (Walkable a [Block' string],
               Walkable a [Inline' string],
               Monad m, Applicative m, Functor m)
           => (a -> m a) -> Block' string -> m (Block' string)
walkBlockM f (Para xs)                = Para <$> walkM f xs
walkBlockM f (Plain xs)               = Plain <$> walkM f xs
walkBlockM f (LineBlock xs)           = LineBlock <$> walkM f xs
walkBlockM f (BlockQuote xs)          = BlockQuote <$> walkM f xs
walkBlockM f (OrderedList a cs)       = OrderedList a <$> walkM f cs
walkBlockM f (BulletList cs)          = BulletList <$> walkM f cs
walkBlockM f (DefinitionList xs)      = DefinitionList <$> walkM f xs
walkBlockM f (Header lev attr xs)     = Header lev attr <$> walkM f xs
walkBlockM f (Div attr bs')           = Div attr <$> walkM f bs'
walkBlockM _ x@CodeBlock {}           = return x
walkBlockM _ x@RawBlock {}            = return x
walkBlockM _ HorizontalRule           = return HorizontalRule
walkBlockM _ Null                     = return Null
walkBlockM f (Table capt as ws hs rs) = do capt' <- walkM f capt
                                           hs' <- walkM f hs
                                           rs' <- walkM f rs
                                           return $ Table capt' as ws hs' rs'

walkMetaValueM :: (Walkable a (MetaValue' string), Walkable a [Block' string],
                  Walkable a [Inline' string], Monad f, Applicative f, Functor f)
               => (a -> f a) -> MetaValue' string -> f (MetaValue' string)
walkMetaValueM f (MetaList xs)    = MetaList <$> walkM f xs
walkMetaValueM _ (MetaBool b)     = return $ MetaBool b
walkMetaValueM _ (MetaString s)   = return $ MetaString s
walkMetaValueM f (MetaInlines xs) = MetaInlines <$> walkM f xs
walkMetaValueM f (MetaBlocks bs)  = MetaBlocks <$> walkM f bs
walkMetaValueM f (MetaMap m)      = MetaMap <$> walkM f m

queryInline :: (Walkable a (Citation' string), Walkable a [Block' string],
                Walkable a [Inline' string], Monoid c)
            => (a -> c) -> (Inline' string) -> c
queryInline _ (Str _)         = mempty
queryInline f (Emph xs)       = query f xs
queryInline f (Strong xs)     = query f xs
queryInline f (Strikeout xs)  = query f xs
queryInline f (Subscript xs)  = query f xs
queryInline f (Superscript xs)= query f xs
queryInline f (SmallCaps xs)  = query f xs
queryInline f (Quoted _ xs)   = query f xs
queryInline f (Cite cs xs)    = query f cs <> query f xs
queryInline _ (Code _ _)      = mempty
queryInline _ Space           = mempty
queryInline _ SoftBreak       = mempty
queryInline _ LineBreak       = mempty
queryInline _ (Math _ _)      = mempty
queryInline _ (RawInline _ _) = mempty
queryInline f (Link _ xs _)   = query f xs
queryInline f (Image _ xs _)  = query f xs
queryInline f (Note bs)       = query f bs
queryInline f (Span _ xs)     = query f xs

queryBlock :: (Walkable a (Citation' string), Walkable a [Block' string],
                Walkable a [Inline' string], Monoid c)
           => (a -> c) -> Block' string -> c
queryBlock f (Para xs)                = query f xs
queryBlock f (Plain xs)               = query f xs
queryBlock f (LineBlock xs)           = query f xs
queryBlock _ (CodeBlock _ _)          = mempty
queryBlock _ (RawBlock _ _)           = mempty
queryBlock f (BlockQuote bs)          = query f bs
queryBlock f (OrderedList _ cs)       = query f cs
queryBlock f (BulletList cs)          = query f cs
queryBlock f (DefinitionList xs)      = query f xs
queryBlock f (Header _ _ xs)          = query f xs
queryBlock _ HorizontalRule           = mempty
queryBlock f (Table capt _ _ hs rs)   = query f capt <> query f hs <> query f rs
queryBlock f (Div _ bs)               = query f bs
queryBlock _ Null                     = mempty

queryMetaValue :: (Walkable a (MetaValue' string), Walkable a [Block' string],
                   Walkable a [Inline' string], Monoid c)
               => (a -> c) -> MetaValue' string -> c
queryMetaValue f (MetaList xs)    = query f xs
queryMetaValue _ (MetaBool _)     = mempty
queryMetaValue _ (MetaString _)   = mempty
queryMetaValue f (MetaInlines xs) = query f xs
queryMetaValue f (MetaBlocks bs)  = query f bs
queryMetaValue f (MetaMap m)      = query f m

walkCitationM :: (Walkable a [Inline' string], Monad m, Applicative m, Functor m)
              => (a -> m a) -> Citation' string -> m (Citation' string)
walkCitationM f (Citation id' pref suff mode notenum hash) =
    do pref' <- walkM f pref
       suff' <- walkM f suff
       return $ Citation id' pref' suff' mode notenum hash

queryCitation :: (Walkable a [Inline' string], Monoid c)
              => (a -> c) -> Citation' string -> c
queryCitation f (Citation _ pref suff _ _ _) = query f pref <> query f suff

walkPandocM :: (Walkable a (Meta' string), Walkable a [Block' string], Monad m,
                  Applicative m, Functor m)
            => (a -> m a) -> Pandoc' string -> m (Pandoc' string)
walkPandocM f (Pandoc m bs) = do m' <- walkM f m
                                 bs' <- walkM f bs
                                 return $ Pandoc m' bs'

queryPandoc :: (Walkable a (Meta' string), Walkable a [Block' string], Monoid c)
             => (a -> c) -> Pandoc' string -> c
queryPandoc f (Pandoc m bs) = query f m <> query f bs
