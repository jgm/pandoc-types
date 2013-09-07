{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Generic
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Generic functions for manipulating 'Pandoc' documents.
(Note:  the functions defined in @Text.Pandoc.Walk@ should be used instead,
when possible, as they are much faster.)

Here's a simple example, defining a function that replaces all the level 3+
headers in a document with regular paragraphs in ALL CAPS:

> import Text.Pandoc.Definition
> import Text.Pandoc.Generic
> import Data.Char (toUpper)
>
> modHeader :: Block -> Block
> modHeader (Header n _ xs) | n >= 3 = Para $ bottomUp allCaps xs
> modHeader x = x
>
> allCaps :: Inline -> Inline
> allCaps (Str xs) = Str $ map toUpper xs
> allCaps x = x
>
> changeHeaders :: Pandoc -> Pandoc
> changeHeaders = bottomUp modHeader

'bottomUp' is so called because it traverses the @Pandoc@ structure from
bottom up. 'topDown' goes the other way. The difference between them can be
seen from this example:

> normal :: [Inline] -> [Inline]
> normal (Space : Space : xs) = Space : xs
> normal (Emph xs : Emph ys : zs) = Emph (xs ++ ys) : zs
> normal xs = xs
>
> myDoc :: Pandoc
> myDoc =  Pandoc nullMeta
>  [ Para [Str "Hi",Space,Emph [Str "world",Space],Emph [Space,Str "emphasized"]]]

Here we want to use 'topDown' to lift @normal@ to @Pandoc -> Pandoc@.
The top down strategy will collapse the two adjacent @Emph@s first, then
collapse the resulting adjacent @Space@s, as desired. If we used 'bottomUp',
we would end up with two adjacent @Space@s, since the contents of the
two @Emph@ inlines would be processed before the @Emph@s were collapsed
into one.

> topDown normal myDoc ==
>   Pandoc nullMeta
>    [Para [Str "Hi",Space,Emph [Str "world",Space,Str "emphasized"]]]
>
> bottomUp normal myDoc ==
>   Pandoc nullMeta
>    [Para [Str "Hi",Space,Emph [Str "world",Space,Space,Str "emphasized"]]]

'bottomUpM' is a monadic version of 'bottomUp'.  It could be used,
for example, to replace the contents of delimited code blocks with
attribute @include=FILENAME@ with the contents of @FILENAME@:

> doInclude :: Block -> IO Block
> doInclude cb@(CodeBlock (id, classes, namevals) contents) =
>   case lookup "include" namevals of
>        Just f  -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
>        Nothing -> return cb
> doInclude x = return x
>
> processIncludes :: Pandoc -> IO Pandoc
> processIncludes = bottomUpM doInclude

'queryWith' can be used, for example, to compile a list of URLs
linked to in a document:

> extractURL :: Inline -> [String]
> extractURL (Link _ (u,_)) = [u]
> extractURL (Image _ (u,_)) = [u]
> extractURL _ = []
>
> extractURLs :: Pandoc -> [String]
> extractURLs = queryWith extractURL

-}
module Text.Pandoc.Generic where

import Data.Generics
import Data.Monoid

-- | Applies a transformation on @a@s to matching elements in a @b@,
-- moving from the bottom of the structure up.
bottomUp :: (Data a, Data b) => (a -> a) -> b -> b
bottomUp f = everywhere (mkT f)

-- | Applies a transformation on @a@s to matching elements in a @b@,
-- moving from the top of the structure down.
topDown :: (Data a, Data b) => (a -> a) -> b -> b
topDown f = everywhere' (mkT f)

-- | Like 'bottomUp', but with monadic transformations.
bottomUpM :: (Monad m, Data a, Data b) => (a -> m a) -> b -> m b
bottomUpM f = everywhereM (mkM f)

-- | Runs a query on matching @a@ elements in a @c@.  The results
-- of the queries are combined using 'mappend'.
queryWith :: (Data a, Monoid b, Data c) => (a -> b) -> c -> b
queryWith f = everything mappend (mempty `mkQ` f)
