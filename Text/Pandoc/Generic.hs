{-# LANGUAGE CPP #-}
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
   Module      : Text.Pandoc.Generic
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : BSD3

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
> extractURL (Image _ _ (u,_)) = [u]
> extractURL _ = []
>
> extractURLs :: Pandoc -> [String]
> extractURLs = queryWith extractURL

-}
module Text.Pandoc.Generic where

import Data.Generics

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
