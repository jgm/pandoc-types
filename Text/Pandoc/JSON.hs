{-# LANGUAGE FlexibleInstances #-}
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

Example of use:

TODO

-}

module Text.Pandoc.JSON ( module Text.Pandoc.Definition
                        , module Text.Pandoc.Generic
                        , ToJSONFilter(..)
                        )
where
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Data.Maybe (listToMaybe)
import Data.Data
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import System.Environment (getArgs)

-- | 'toJSONFilter' convert a function into a filter that reads pandoc's
-- JSON serialized output from stdin, transforms it by walking the AST
-- and applying the specified function, and serializes the result as JSON
-- to stdout.  Usage example:
--
-- > -- capitalize.hs
-- > -- run with:      pandoc -t json | runghc capitalize.hs | pandoc -f json
-- >
-- > import Text.Pandoc.JSON
-- > import Data.Char (toUpper)
-- >
-- > main :: IO ()
-- > main = toJSONFilter capitalizeStrings
-- >
-- > capitalizeStrings :: Inline -> Inline
-- > capitalizeStrings (Str s) = Str $ map toUpper s
-- > capitalizeStrings x       = x
--
-- The function can be any type @(a -> a)@, @(a -> IO a)@, @(a -> [a])@,
-- or @(a -> IO [a])@, where @a@ is an instance of 'Data'.
-- So, for example, @a@ can be 'Pandoc', 'Inline', 'Block', ['Inline'],
-- ['Block'], 'Meta', 'ListNumberStyle', 'Alignment', 'ListNumberDelim',
-- 'QuoteType', etc. See 'Text.Pandoc.Definition'.

class ToJSONFilter a where
  toJSONFilter :: a -> IO ()

instance (Data a) => ToJSONFilter (a -> a) where
  toJSONFilter f = BL.getContents >>=
    BL.putStr . encode . (bottomUp f :: Pandoc -> Pandoc) . either error id .
    eitherDecode'

instance (Data a) => ToJSONFilter (a -> IO a) where
  toJSONFilter f = BL.getContents >>=
     (bottomUpM f :: Pandoc -> IO Pandoc) . either error id . eitherDecode' >>=
     BL.putStr . encode

instance (Data a) => ToJSONFilter (a -> [a]) where
  toJSONFilter f = BL.getContents >>=
    BL.putStr . encode . (bottomUp (concatMap f) :: Pandoc -> Pandoc) .
      either error id . eitherDecode'

instance (Data a) => ToJSONFilter (a -> IO [a]) where
  toJSONFilter f = BL.getContents >>=
    (bottomUpM (fmap concat . mapM f) :: Pandoc -> IO Pandoc)
      . either error id . eitherDecode' >>=
    BL.putStr . encode

instance (ToJSONFilter a) => ToJSONFilter ([String] -> a) where
  toJSONFilter f = getArgs >>= toJSONFilter . f

instance (ToJSONFilter a) => ToJSONFilter (Maybe String -> a) where
  toJSONFilter f = getArgs >>= toJSONFilter . f . listToMaybe
