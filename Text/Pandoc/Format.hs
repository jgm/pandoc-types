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
  , Formats
  , allFormats
  , formatFromName
  , name
  , singleFormat
  , oneOfFormats
  , noneOfFormats
  , inFormats
  ) where

import Prelude hiding (or)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Aeson ((.=), (.:), FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import Data.Generics (Data, Typeable)
import Data.List (sort)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)

import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Formats
data Formats
  = OneOf (Set Format)
  | NoneOf (Set Format)
  deriving (Eq, Data, Generic, Ord, Read, Show, Typeable)

oneOfFormats :: [Format] -> Formats
oneOfFormats = OneOf . Set.fromList

noneOfFormats :: [Format] -> Formats
noneOfFormats = NoneOf . Set.fromList

inFormats :: Format -> Formats -> Bool
inFormats f (OneOf fs)  = f `Set.member` fs
inFormats f (NoneOf fs) = f `Set.notMember` fs

singleFormat :: Format -> Formats
singleFormat = OneOf . Set.singleton

-- | A known format.
data Format
  = AsciiDoc
  | CommonMark
  | ConTeXt
  | Creole
  | DocBook
  | DokuWiki
  | FB2
  | HTML
  | Haddock
  | ICML
  | Ipynb
  | JATS
  | JSON
  | Jira
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
  | ReStructuredText
  | RTF
  | Roff
  | TEI
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
name ReStructuredText = "rst"
name Txt2tags = "t2t"
name f = T.toLower . T.pack $ show f

-- | List of all formats of which pandoc is aware.
allFormats :: Set Format
allFormats = Set.fromAscList [minBound .. maxBound]

-- | Map from format names to formats. A format may have multiple names.
namedFormat :: Map Text Format
namedFormat = Map.fromList $
  map (\f -> (name f, f)) (Set.toList allFormats)

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

instance ToJSON Formats where
  toJSON (OneOf fs)  = Aeson.object ["oneOf"  .= sort (Set.toList fs)]
  toJSON (NoneOf fs) = Aeson.object ["noneOf" .= sort (Set.toList fs)]

instance FromJSON Formats where
  parseJSON = Aeson.withObject "Formats" $ \obj ->
    (OneOf . Set.fromList <$> obj .: "oneOf") <|>
    (NoneOf . Set.fromList <$> obj .: "noneOf")
    -- <|> object .= "noneOf"

instance NFData Format
instance NFData Formats

