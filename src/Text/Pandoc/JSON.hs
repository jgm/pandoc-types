{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
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
   Module      : Text.Pandoc.JSON
   Copyright   : Copyright (C) 2013-2019 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for serializing the Pandoc AST to JSON and deserializing from JSON.

Example of use:  The following script (@capitalize.hs@) reads
reads a JSON representation of a Pandoc document from stdin,
and writes a JSON representation of a Pandoc document to stdout.
It changes all regular text in the document to uppercase, without
affecting URLs, code, tags, etc.  Run the script with

> pandoc -t json | runghc capitalize.hs | pandoc -f json

or (making capitalize.hs executable)

> pandoc --filter ./capitalize.hs

> #!/usr/bin/env runghc
> import Text.Pandoc.JSON
> import Data.Char (toUpper)
>
> main :: IO ()
> main = toJSONFilter capitalizeStrings
>
> capitalizeStrings :: Inline -> Inline
> capitalizeStrings (Str s) = Str $ map toUpper s
> capitalizeStrings x       = x

-}

module Text.Pandoc.JSON ( module Text.Pandoc.Definition
                        , ToJSONFilter(..)
                        )
where
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Aeson
import System.Environment (getArgs)

-- | 'toJSONFilter' convert a function into a filter that reads pandoc's
-- JSON serialized output from stdin, transforms it by walking the AST
-- and applying the specified function, and serializes the result as JSON
-- to stdout.
--
-- For a straight transformation, use a function of type @a -> a@ or
-- @a -> IO a@ where @a@ = 'Block', 'Inline','Pandoc', 'Meta', or 'MetaValue'.
--
-- If your transformation needs to be sensitive to the script's arguments,
-- use a function of type @[String] -> a -> a@ (with @a@ constrained as above).
-- The @[String]@ will be populated with the script's arguments.
--
-- An alternative is to use the type @Maybe Format -> a -> a@.
-- This is appropriate when the first argument of the script (if present)
-- will be the target format, and allows scripts to behave differently
-- depending on the target format.  The pandoc executable automatically
-- provides the target format as argument when scripts are called using
-- the `--filter` option.

class ToJSONFilter a where
  toJSONFilter :: a -> IO ()

instance (Walkable a Pandoc) => ToJSONFilter (a -> a) where
  toJSONFilter f = BL.getContents >>=
    BL.putStr . encode . (walk f :: Pandoc -> Pandoc) . either error id .
    eitherDecode'

instance (Walkable a Pandoc) => ToJSONFilter (a -> IO a) where
  toJSONFilter f = BL.getContents >>=
     (walkM f :: Pandoc -> IO Pandoc) . either error id . eitherDecode' >>=
     BL.putStr . encode

instance (Walkable [a] Pandoc) => ToJSONFilter (a -> [a]) where
  toJSONFilter f = BL.getContents >>=
    BL.putStr . encode . (walk (concatMap f) :: Pandoc -> Pandoc) .
    either error id . eitherDecode'

instance (Walkable [a] Pandoc) => ToJSONFilter (a -> IO [a]) where
  toJSONFilter f = BL.getContents >>=
     (walkM (fmap concat . mapM f) :: Pandoc -> IO Pandoc) .
     either error id . eitherDecode' >>=
     BL.putStr . encode

instance (ToJSONFilter a) => ToJSONFilter ([String] -> a) where
  toJSONFilter f = getArgs >>= toJSONFilter . f

instance (ToJSONFilter a) => ToJSONFilter (Maybe Format -> a) where
  toJSONFilter f = getArgs >>= toJSONFilter . f . fmap (Format . T.pack) . listToMaybe
