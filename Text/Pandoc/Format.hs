{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Definition
   Copyright   : © 2006-2019 John MacFarlane, Albert Krewinkel
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Set of known formats.
-}
module Text.Pandoc.Format
  ( Format (..)
  , allFormats
  , formatFromName
  , name
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import Data.Generics (Data, Typeable)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)

import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | A known format.
data Format
  = AsciiDoc
  | Beamer
  | CommonMark
  | ConTeXt
  | Creole
  | DZSlides
  | DocBook4
  | DocBook5
  | Docx
  | DokuWiki
  | EPUB2
  | EPUB3
  | FB2
  | GFM
  | HTML4
  | HTML5
  | Haddock
  | ICML
  | Ipynb
  | JATS
  | JSON
  | LaTeX
  | Man
  | Markdown
  | MediaWiki
  | MS
  | Muse
  | Native
  | ODT
  | OOXML
  | OPML
  | OpenDocument
  | Org
  | PlainText
  | Pptx
  | RST
  | RTF
  | RevealJS
  | Roff
  | S5
  | Slideous
  | Slidy
  | TEI
  | TeX           -- This is a kludge to subsume ConTeXt and LaTeX. It should be
                  -- removed when there are more convenient ways of dealing with
                  -- this.
  | TWiki
  | Texinfo
  | Textile
  | TikiWiki
  | Txt2tags
  | Vimwiki
  | ZimWiki
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

-- | Short name of the format.
name :: Format -> Text
name PlainText = "plain"
name Txt2tags = "t2t"
name f = T.toLower . T.pack $ show f

-- | List of all formats of which pandoc is aware.
allFormats :: Set Format
allFormats = Set.fromAscList [minBound .. maxBound]

-- | Map from format names to formats. A format may have multiple names.
namedFormat :: Map Text Format
namedFormat = Map.fromList $
  [ ("docbook", DocBook5)
  , ("epub", EPUB3)
  , ("html", HTML5)
  , ("plain", PlainText)
  , ("t2t", Txt2tags)
  ] ++
  map (\f -> (T.toLower . T.pack $ show f, f)) (Set.toList allFormats)

-- | Get a format from a string identifier.
formatFromName :: Text -> Maybe Format
formatFromName = flip Map.lookup namedFormat

-- | Use just the format's name to represent it in JSON; fully backwards
-- compatible.
instance ToJSON Format where
  toJSON = toJSON . name

-- | Read Format from a string; only partly backwards compatible, as unknown
-- formats no longer work.
instance FromJSON Format where
  parseJSON = Aeson.withText "Format" $ \t ->
    case formatFromName t of
      Just f  -> return f
      Nothing -> fail ("unknown format: " ++ T.unpack t)

instance NFData Format
