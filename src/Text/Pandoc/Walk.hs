{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints -O2 #-}
#endif
#define OVERLAPS {-# OVERLAPPING #-}
{-
Copyright (c) 2013-2019, John MacFarlane

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
   Copyright   : Copyright (C) 2013-2019 John MacFarlane
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

> extractURL :: Inline -> [Text]
> extractURL (Link _ _ (u,_)) = [u]
> extractURL (Image _ _ (u,_)) = [u]
> extractURL _ = []
>
> extractURLs :: Pandoc -> [Text]
> extractURLs = query extractURL
-}


module Text.Pandoc.Walk
  ( Walkable(..)
  , queryBlock
  , queryCaption
  , queryRow
  , queryTableHead
  , queryTableBody
  , queryTableFoot
  , queryCell
  , queryCitation
  , queryInline
  , queryMetaValue
  , queryMetaValue'
  , queryPandoc
  , walkBlockM
  , walkCaptionM
  , walkRowM
  , walkTableHeadM
  , walkTableBodyM
  , walkTableFootM
  , walkCellM
  , walkCitationM
  , walkInlineM
  , walkMetaValueM
  , walkMetaValueM'
  , walkPandocM
  )
where
import Control.Applicative (Applicative ((<*>), pure), (<$>))
import Control.Monad ((>=>))
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map as M
import Text.Pandoc.Definition
import qualified Text.Pandoc.Definition.Functors as F
import qualified Data.Traversable as T
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import Data.Foldable (Foldable)
import Data.Monoid ((<>))

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

instance {-# OVERLAPPABLE #-}
        (Foldable t, Traversable t, Walkable a b) => Walkable a (t b) where
  walk f  = T.fmapDefault (walk f)
  walkM f = T.mapM (walkM f)
  query f = F.foldMap (query f)

-- Walk pairs by handling both elements, then combine the results.
instance OVERLAPS
        (Walkable a b, Walkable a c) => Walkable a (b,c) where
  walk f (x,y)  = (walk f x, walk f y)
  walkM f (x,y) = do x' <- walkM f x
                     y' <- walkM f y
                     return (x',y')
  query f (x,y) = mappend (query f x) (query f y)

-- instance {-# OVERLAPPABLE #-} Walkable a a where
--   walkM f x = f x
--   query f x = f x

instance Walkable Inline Inline where
  walkM f x = walkInlineM f x >>= f
  query f x = f x <> queryInline f x

instance OVERLAPS
         Walkable [Inline] [Inline] where
  walkM f = T.traverse (walkInlineM f) >=> f
  query f inlns = f inlns <> mconcat (map (queryInline f) inlns)

instance Walkable [Inline] Inline where
  walkM = walkInlineM
  query = queryInline

instance Walkable Inline Block where
  walkM = walkBlockM
  query = queryBlock

instance Walkable [Inline] Block where
  walkM = walkBlockM
  query = queryBlock

instance Walkable Block Block where
  walkM f x = walkBlockM f x >>= f
  query f x = f x <> queryBlock f x

instance Walkable [Block] Block where
  walkM = walkBlockM
  query = queryBlock

instance OVERLAPS
         Walkable [Block] [Block] where
  walkM f = T.traverse (walkBlockM f) >=> f
  query f blks = f blks <> mconcat (map (queryBlock f) blks)

instance Walkable Block Inline where
  walkM = walkInlineM
  query = queryInline

instance Walkable [Block] Inline where
  walkM = walkInlineM
  query = queryInline

--
-- Walk Pandoc
--
instance Walkable Block Pandoc where
  walkM = walkPandocM
  query = queryPandoc

instance Walkable [Block] Pandoc where
  walkM = walkPandocM
  query = queryPandoc

instance Walkable Inline Pandoc where
  walkM = walkPandocM
  query = queryPandoc

instance Walkable [Inline] Pandoc where
  walkM = walkPandocM
  query = queryPandoc

instance Walkable Meta Pandoc where
  walkM f (Pandoc m bs) = Pandoc <$> f m <*> pure bs
  query f (Pandoc m _) = f m

instance Walkable MetaValue Pandoc where
  walkM f (Pandoc m bs) = Pandoc <$> walkM f m <*> pure bs
  query f (Pandoc m _) = query f m

instance Walkable Pandoc Pandoc where
  walkM f = f
  query f = f

--
-- Walk Meta
--
instance Walkable Meta Meta where
  walkM f = f
  query f = f

instance Walkable Inline Meta where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance Walkable [Inline] Meta where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance Walkable Block Meta where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance Walkable [Block] Meta where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

instance Walkable MetaValue Meta where
  walkM f (Meta metamap) =
    Meta . M.fromAscList <$> mapM (\(k, v) -> (,) k <$> walkM f v) (M.toAscList metamap)
  query f (Meta metamap) = M.foldMapWithKey (const $ query f) metamap

--
-- Walk MetaValue
--
instance Walkable MetaValue MetaValue where
  walkM f x = walkMetaValueM' f x >>= f
  query f x = f x <> queryMetaValue' f x

instance Walkable Inline MetaValue where
  walkM = walkMetaValueM
  query = queryMetaValue

instance Walkable [Inline] MetaValue where
  walkM = walkMetaValueM
  query = queryMetaValue

instance Walkable Block MetaValue where
  walkM = walkMetaValueM
  query = queryMetaValue

instance Walkable [Block] MetaValue where
  walkM = walkMetaValueM
  query = queryMetaValue

--
-- Walk Row
--
instance Walkable Inline Row where
  walkM = walkRowM
  query = queryRow

instance Walkable [Inline] Row where
  walkM = walkRowM
  query = queryRow

instance Walkable Block Row where
  walkM = walkRowM
  query = queryRow

instance Walkable [Block] Row where
  walkM = walkRowM
  query = queryRow

--
-- Walk TableHead
--
instance Walkable Inline TableHead where
  walkM = walkTableHeadM
  query = queryTableHead

instance Walkable [Inline] TableHead where
  walkM = walkTableHeadM
  query = queryTableHead

instance Walkable Block TableHead where
  walkM = walkTableHeadM
  query = queryTableHead

instance Walkable [Block] TableHead where
  walkM = walkTableHeadM
  query = queryTableHead

--
-- Walk TableBody
--
instance Walkable Inline TableBody where
  walkM = walkTableBodyM
  query = queryTableBody

instance Walkable [Inline] TableBody where
  walkM = walkTableBodyM
  query = queryTableBody

instance Walkable Block TableBody where
  walkM = walkTableBodyM
  query = queryTableBody

instance Walkable [Block] TableBody where
  walkM = walkTableBodyM
  query = queryTableBody

--
-- Walk TableFoot
--
instance Walkable Inline TableFoot where
  walkM = walkTableFootM
  query = queryTableFoot

instance Walkable [Inline] TableFoot where
  walkM = walkTableFootM
  query = queryTableFoot

instance Walkable Block TableFoot where
  walkM = walkTableFootM
  query = queryTableFoot

instance Walkable [Block] TableFoot where
  walkM = walkTableFootM
  query = queryTableFoot

--
-- Walk Caption
--
instance Walkable Inline Caption where
  walkM = walkCaptionM
  query = queryCaption

instance Walkable [Inline] Caption where
  walkM = walkCaptionM
  query = queryCaption

instance Walkable Block Caption where
  walkM = walkCaptionM
  query = queryCaption

instance Walkable [Block] Caption where
  walkM = walkCaptionM
  query = queryCaption

--
-- Walk Cell
--
instance Walkable Inline Cell where
  walkM = walkCellM
  query = queryCell

instance Walkable [Inline] Cell where
  walkM = walkCellM
  query = queryCell

instance Walkable Block Cell where
  walkM = walkCellM
  query = queryCell

instance Walkable [Block] Cell where
  walkM = walkCellM
  query = queryCell

--
-- Walk Citation
--
instance Walkable Inline Citation where
  walkM = walkCitationM
  query = queryCitation

instance Walkable [Inline] Citation where
  walkM = walkCitationM
  query = queryCitation

instance Walkable Block Citation where
  walkM = walkCitationM
  query = queryCitation

instance Walkable [Block] Citation where
  walkM = walkCitationM
  query = queryCitation

-- | Helper method to walk to elements nested below @'Inline'@ nodes.
--
-- When walking an inline with this function, only the contents of the traversed
-- inline element may change. The element itself, i.e. its constructor, cannot
-- be changed.
walkInlineM :: (Walkable a Citation, Walkable a [Block],
                Walkable a [Inline], Monad m, Applicative m, Functor m)
            => (a -> m a) -> Inline -> m Inline
walkInlineM f (Inline i) = Inline <$> walkInlineFM f i

walkInlineFM :: (Walkable a (F.Citation inline), Walkable a [block],
                 Walkable a [inline], Monad m, Applicative m, Functor m)
              => (a -> m a) -> F.Inline block inline -> m (F.Inline block inline)
walkInlineFM _ (F.Str xs)         = return (F.Str xs)
walkInlineFM f (F.Emph xs)        = F.Emph <$> walkM f xs
walkInlineFM f (F.Underline xs)   = F.Underline <$> walkM f xs
walkInlineFM f (F.Strong xs)      = F.Strong <$> walkM f xs
walkInlineFM f (F.Strikeout xs)   = F.Strikeout <$> walkM f xs
walkInlineFM f (F.Subscript xs)   = F.Subscript <$> walkM f xs
walkInlineFM f (F.Superscript xs) = F.Superscript <$> walkM f xs
walkInlineFM f (F.SmallCaps xs)   = F.SmallCaps <$> walkM f xs
walkInlineFM f (F.Quoted qt xs)   = F.Quoted qt <$> walkM f xs
walkInlineFM f (F.Link atr xs t)  = F.Link atr <$> walkM f xs <*> pure t
walkInlineFM f (F.Image atr xs t) = F.Image atr <$> walkM f xs <*> pure t
walkInlineFM f (F.Note bs)        = F.Note <$> walkM f bs
walkInlineFM f (F.Span attr xs)   = F.Span attr <$> walkM f xs
walkInlineFM f (F.Cite cs xs)     = F.Cite <$> walkM f cs <*> walkM f xs
walkInlineFM _ F.LineBreak        = return F.LineBreak
walkInlineFM _ F.SoftBreak        = return F.SoftBreak
walkInlineFM _ F.Space            = return F.Space
walkInlineFM _ x@F.Code {}        = return x
walkInlineFM _ x@F.Math {}        = return x
walkInlineFM _ x@F.RawInline {}   = return x

-- | Perform a query on elements nested below an @'Inline'@ element by
-- querying nested lists of @Inline@s, @Block@s, or @Citation@s.
queryInline :: (Walkable a Citation, Walkable a [Block],
                Walkable a [Inline], Monoid c)
            => (a -> c) -> Inline -> c
queryInline f (Inline i) = queryInlineF f i

queryInlineF :: (Walkable a (F.Citation inline), Walkable a [block],
                 Walkable a [inline], Monoid c)
             => (a -> c) -> F.Inline block inline -> c
queryInlineF _ (F.Str _)         = mempty
queryInlineF f (F.Emph xs)       = query f xs
queryInlineF f (F.Underline xs)  = query f xs
queryInlineF f (F.Strong xs)     = query f xs
queryInlineF f (F.Strikeout xs)  = query f xs
queryInlineF f (F.Subscript xs)  = query f xs
queryInlineF f (F.Superscript xs)= query f xs
queryInlineF f (F.SmallCaps xs)  = query f xs
queryInlineF f (F.Quoted _ xs)   = query f xs
queryInlineF f (F.Cite cs xs)    = query f cs <> query f xs
queryInlineF _ (F.Code _ _)      = mempty
queryInlineF _ F.Space           = mempty
queryInlineF _ F.SoftBreak       = mempty
queryInlineF _ F.LineBreak       = mempty
queryInlineF _ (F.Math _ _)      = mempty
queryInlineF _ (F.RawInline _ _) = mempty
queryInlineF f (F.Link _ xs _)   = query f xs
queryInlineF f (F.Image _ xs _)  = query f xs
queryInlineF f (F.Note bs)       = query f bs
queryInlineF f (F.Span _ xs)     = query f xs


-- | Helper method to walk to elements nested below @'Block'@ nodes.
--
-- When walking a block with this function, only the contents of the traversed
-- block element may change. The element itself, i.e. its constructor, its @'Attr'@,
-- and its raw text value, will remain unchanged.
walkBlockM :: (Walkable a [Block], Walkable a [Inline], Walkable a Row,
               Walkable a Caption, Walkable a TableHead, Walkable a TableBody,
               Walkable a TableFoot, Monad m, Applicative m, Functor m)
           => (a -> m a) -> Block -> m Block
walkBlockM f (Block b) = Block <$> walkBlockFM f b

walkBlockFM :: (Monad m, Walkable a (F.Caption inline block),
                Walkable a [inline], Walkable a [block],
                Walkable a (F.TableHead block), Walkable a (F.TableBody block),
                Walkable a (F.TableFoot block))
            => (a -> m a) -> F.Block inline block -> m (F.Block inline block)
walkBlockFM f (F.Para xs)                = F.Para <$> walkM f xs
walkBlockFM f (F.Plain xs)               = F.Plain <$> walkM f xs
walkBlockFM f (F.LineBlock xs)           = F.LineBlock <$> walkM f xs
walkBlockFM f (F.BlockQuote xs)          = F.BlockQuote <$> walkM f xs
walkBlockFM f (F.OrderedList a cs)       = F.OrderedList a <$> walkM f cs
walkBlockFM f (F.BulletList cs)          = F.BulletList <$> walkM f cs
walkBlockFM f (F.DefinitionList xs)      = F.DefinitionList <$> walkM f xs
walkBlockFM f (F.Header lev attr xs)     = F.Header lev attr <$> walkM f xs
walkBlockFM f (F.Div attr bs')           = F.Div attr <$> walkM f bs'
walkBlockFM _ x@F.CodeBlock {}           = return x
walkBlockFM _ x@F.RawBlock {}            = return x
walkBlockFM _ F.HorizontalRule           = return F.HorizontalRule
walkBlockFM _ F.Null                     = return F.Null
walkBlockFM f (F.Table attr capt as hs bs fs)
  = do capt' <- walkM f capt
       hs' <- walkM f hs
       bs' <- walkM f bs
       fs' <- walkM f fs
       return $ F.Table attr capt' as hs' bs' fs'

-- | Perform a query on elements nested below a @'Block'@ element by
-- querying all directly nested lists of @Inline@s or @Block@s.
queryBlock :: (Walkable a Citation, Walkable a [Block], Walkable a Row,
               Walkable a Caption, Walkable a TableHead, Walkable a TableBody,
               Walkable a TableFoot, Walkable a [Inline], Monoid c)
           => (a -> c) -> Block -> c
queryBlock f (Block b) = queryBlockF f b

queryBlockF :: (Monoid c, Walkable a (F.Caption inline block),
                Walkable a [inline], Walkable a [block],
                Walkable a (F.TableHead block), Walkable a (F.TableBody block),
                Walkable a (F.TableFoot block))
            => (a -> c) -> F.Block inline block -> c
queryBlockF f (F.Para xs)                = query f xs
queryBlockF f (F.Plain xs)               = query f xs
queryBlockF f (F.LineBlock xs)           = query f xs
queryBlockF _ (F.CodeBlock _ _)          = mempty
queryBlockF _ (F.RawBlock _ _)           = mempty
queryBlockF f (F.BlockQuote bs)          = query f bs
queryBlockF f (F.OrderedList _ cs)       = query f cs
queryBlockF f (F.BulletList cs)          = query f cs
queryBlockF f (F.DefinitionList xs)      = query f xs
queryBlockF f (F.Header _ _ xs)          = query f xs
queryBlockF _ F.HorizontalRule           = mempty
queryBlockF f (F.Table _ capt _ hs bs fs)
  = query f capt <>
    query f hs <>
    query f bs <>
    query f fs
queryBlockF f (F.Div _ bs)               = query f bs
queryBlockF _ F.Null                     = mempty

-- | Helper method to walk to elements nested below @'MetaValue'@ nodes.
--
-- When walking a meta value with this function, only the contents of the
-- traversed meta value element may change. @MetaBool@ and @MetaString@ will
-- always remain unchanged.
walkMetaValueM :: (Walkable a MetaValue, Walkable a [Block],
                  Walkable a [Inline], Monad f, Applicative f, Functor f)
               => (a -> f a) -> MetaValue -> f MetaValue
walkMetaValueM f (MetaValue b) = MetaValue <$> walkMetaValueFM f b

walkMetaValueFM :: (Monad f, Walkable a metaValue,
                    Walkable a [metaValue], Walkable a [inline], Walkable a [block])
                => (a -> f a)
                -> F.MetaValue inline block metaValue
                -> f (F.MetaValue inline block metaValue)
walkMetaValueFM f (F.MetaList xs)    = F.MetaList <$> walkM f xs
walkMetaValueFM _ (F.MetaBool b)     = return $ F.MetaBool b
walkMetaValueFM _ (F.MetaString s)   = return $ F.MetaString s
walkMetaValueFM f (F.MetaInlines xs) = F.MetaInlines <$> walkM f xs
walkMetaValueFM f (F.MetaBlocks bs)  = F.MetaBlocks <$> walkM f bs
walkMetaValueFM f (F.MetaMap m)      = F.MetaMap <$> walkM f m

-- | Helper method to walk @'MetaValue'@ nodes nested below @'MetaValue'@ nodes.
walkMetaValueM' :: (Monad f, Applicative f, Functor f)
                => (MetaValue -> f MetaValue) -> MetaValue -> f MetaValue
walkMetaValueM' f (MetaValue b) = MetaValue <$> walkMetaValueFM' f b

walkMetaValueFM' :: (Monad f, Walkable a metaValue)
                 => (a -> f a)
                 -> F.MetaValue inline block metaValue
                 -> f (F.MetaValue inline block metaValue)
walkMetaValueFM' f (F.MetaMap m) =
    F.MetaMap . M.fromAscList <$> mapM (\(k, v) -> (,) k <$> walkM f v) (M.toAscList m)
walkMetaValueFM' f (F.MetaList xs) = F.MetaList <$> mapM (walkM f) xs
walkMetaValueFM' _ x = return x

-- | Perform a query on elements nested below a @'MetaValue'@ element by
-- querying all directly nested lists of @Inline@s, list of @Block@s, or
-- lists or maps of @MetaValue@s.
queryMetaValue :: (Walkable a MetaValue, Walkable a [Block],
                   Walkable a [Inline], Monoid c)
               => (a -> c) -> MetaValue -> c
queryMetaValue f (MetaValue b) = queryMetaValueF f b

queryMetaValueF :: (Monoid c, Walkable a metaValue,
                    Walkable a [metaValue], Walkable a [inline], Walkable a [block])
                => (a -> c) -> F.MetaValue inline block metaValue -> c
queryMetaValueF f (F.MetaList xs)    = query f xs
queryMetaValueF _ (F.MetaBool _)     = mempty
queryMetaValueF _ (F.MetaString _)   = mempty
queryMetaValueF f (F.MetaInlines xs) = query f xs
queryMetaValueF f (F.MetaBlocks bs)  = query f bs
queryMetaValueF f (F.MetaMap m)      = query f m

-- | Perform a query on @'MetaValue'@ elements nested below a @'MetaValue'@
-- element
queryMetaValue' :: Monoid c
                => (MetaValue -> c) -> MetaValue -> c
queryMetaValue' f (MetaValue b) = queryMetaValue'F f b

queryMetaValue'F :: (Monoid c, Walkable a b)
                 => (a -> c) -> F.MetaValue inline block b -> c
queryMetaValue'F f (F.MetaMap m)   = M.foldMapWithKey (const $ query f) m
queryMetaValue'F f (F.MetaList xs) = mconcat $ map (query f) xs
queryMetaValue'F _ _               = mempty

-- | Helper method to walk to elements nested below @'Citation'@ nodes.
--
-- The non-inline contents of a citation will remain unchanged during traversal.
-- Only the inline contents, viz. the citation's prefix and postfix, will be
-- traversed further and can thus be changed during this operation.
walkCitationM :: (Walkable a [inline], Monad m, Applicative m, Functor m)
              => (a -> m a) -> F.Citation inline -> m (F.Citation inline)
walkCitationM f (Citation id' pref suff mode notenum hash) =
    do pref' <- walkM f pref
       suff' <- walkM f suff
       return $ Citation id' pref' suff' mode notenum hash

-- | Perform a query on elements nested below a @'Citation'@ element by
-- querying the prefix and postfix @Inline@ lists.
queryCitation :: (Walkable a [inline], Monoid c)
              => (a -> c) -> F.Citation inline -> c
queryCitation f (Citation _ pref suff _ _ _) = query f pref <> query f suff

-- | Helper method to walk the elements nested below @'Row'@ nodes. The
-- @'Attr'@ component is not changed by this operation.
walkRowM :: (Walkable a Cell, Monad m)
         => (a -> m a) -> Row -> m Row
walkRowM f (Row attr bd) = Row attr <$> walkM f bd

-- | Query the elements below a 'Row' element.
queryRow :: (Walkable a Cell, Monoid c)
         => (a -> c) -> Row -> c
queryRow f (Row _ bd) = query f bd

-- | Helper method to walk the elements nested below @'TableHead'@ nodes. The
-- @'Attr'@ component is not changed by this operation.
walkTableHeadM :: (Walkable a Row, Monad m)
               => (a -> m a) -> TableHead -> m TableHead
walkTableHeadM f (TableHead attr body) = TableHead attr <$> walkM f body

-- | Query the elements below a 'TableHead' element.
queryTableHead :: (Walkable a Row, Monoid c)
               => (a -> c) -> TableHead -> c
queryTableHead f (TableHead _ body) = query f body

-- | Helper method to walk the elements nested below @'TableBody'@
-- nodes. The @'Attr'@ and @'RowHeadColumns'@ components are not
-- changed by this operation.
walkTableBodyM :: (Walkable a Row, Monad m)
               => (a -> m a) -> TableBody -> m TableBody
walkTableBodyM f (TableBody attr rhc hd bd) = TableBody attr rhc <$> walkM f hd <*> walkM f bd

-- | Query the elements below a 'TableBody' element.
queryTableBody :: (Walkable a Row, Monoid c)
               => (a -> c) -> TableBody -> c
queryTableBody f (TableBody _ _ hd bd) = query f hd <> query f bd

-- | Helper method to walk the elements nested below @'TableFoot'@ nodes. The
-- @'Attr'@ component is not changed by this operation.
walkTableFootM :: (Walkable a Row, Monad m)
               => (a -> m a) -> TableFoot -> m TableFoot
walkTableFootM f (TableFoot attr body) = TableFoot attr <$> walkM f body

-- | Query the elements below a 'TableFoot' element.
queryTableFoot :: (Walkable a Row, Monoid c)
               => (a -> c) -> TableFoot -> c
queryTableFoot f (TableFoot _ body) = query f body

-- | Helper method to walk the elements nested below 'Cell'
-- nodes. Only the @['Block']@ cell content is changed by this
-- operation.
walkCellM :: (Walkable a [Block], Monad m)
          => (a -> m a) -> Cell -> m Cell
walkCellM f (Cell attr ma rs cs content) = Cell attr ma rs cs <$> walkM f content

-- | Query the elements below a 'Cell' element.
queryCell :: (Walkable a [Block], Monoid c)
          => (a -> c) -> Cell -> c
queryCell f (Cell _ _ _ _ content) = query f content

-- | Helper method to walk the elements nested below 'Caption'
-- nodes.
walkCaptionM :: (Walkable a [Block], Walkable a [Inline], Monad m, Walkable a ShortCaption)
          => (a -> m a) -> Caption -> m Caption
walkCaptionM f (Caption mshort body) = Caption <$> walkM f mshort <*> walkM f body

-- | Query the elements below a 'Cell' element.
queryCaption :: (Walkable a [Block], Walkable a [Inline], Walkable a ShortCaption, Monoid c)
          => (a -> c) -> Caption -> c
queryCaption f (Caption mshort body) = query f mshort <> query f body

-- | Helper method to walk the components of a Pandoc element.
walkPandocM :: (Walkable a Meta, Walkable a [Block], Monad m,
                  Applicative m, Functor m)
            => (a -> m a) -> Pandoc -> m Pandoc
walkPandocM f (Pandoc m bs) = do m' <- walkM f m
                                 bs' <- walkM f bs
                                 return $ Pandoc m' bs'

-- | Query a pandoc element by recursing first into its @'Meta'@ data
-- and then append the result of recursing into the list of @'Block'@s.
queryPandoc :: (Walkable a Meta, Walkable a [Block], Monoid c)
             => (a -> c) -> Pandoc -> c
queryPandoc f (Pandoc m bs) = query f m <> query f bs
