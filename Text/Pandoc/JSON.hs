{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
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
   Module      : Text.Pandoc.JSON
   Copyright   : Copyright (C) 2013 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for serializing the Pandoc AST to JSON and deserializing from JSON.

Example of use:  The following script (`capitalize.hs`) reads
reads a JSON representation of a Pandoc document from stdin,
and writes a JSON representation of a Pandoc document to stdout.
It changes all regular text in the document to uppercase, without
affecting URLs, code, tags, etc.  Run the script with

> pandoc -t json | runghc capitalize.hs | pandoc -f json

or (making capitalize.hs executable)

> pandoc --filter ./capitalize.hs

> -- capitalize.hs
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
import Data.Data
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import System.Environment (getArgs)

-- | 'toJSONFilter' convert a function into a filter that reads pandoc's
-- JSON serialized output from stdin, transforms it by walking the AST
-- and applying the specified function, and serializes the result as JSON
-- to stdout.
--
-- The function can be of type @(a -> a)@ or @(a -> IO a)@, for
-- @a@ = 'Block', 'Inline', 'Pandoc', 'Meta', or 'MetaValue'.

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

instance (ToJSONFilter a) => ToJSONFilter ([String] -> a) where
  toJSONFilter f = getArgs >>= toJSONFilter . f

instance (ToJSONFilter a) => ToJSONFilter (Maybe String -> a) where
  toJSONFilter f = getArgs >>= toJSONFilter . f . listToMaybe
