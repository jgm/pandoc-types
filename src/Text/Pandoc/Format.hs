{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Text.Pandoc.Format
   Copyright   : Copyright 2020 Christian Despres
   License     : BSD3

   Maintainer  : Christian Despres <christian.j.j.despres@gmail.com>
   Stability   : alpha
   Portability : portable

Definition of a 'KnownFormat' and related types that enumerate the
formats that Pandoc recognizes, and a 'Formats' type and combinators
to implement 'Format' patterns and matching. This module was designed
to have its functions imported qualified.
-}

module Text.Pandoc.Format
  ( -- * The 'Format' types
    Format(..)
  , toKnownFormat
  , fromKnownFormat
  , KnownFormat(..)
  , castsToKnown
  , castsTo

  -- * 'Formats' patterns
  -- $formats
  , Formats(..)
  , ToFormats(..)
  , anyOf
  , noneOf
  , anything
  , nothing
  , and
  , or
  , except
  , not

  -- * Matching 'Formats' patterns
  , matches
  , knownMatches

  -- * Direct 'Formats' conversions
  , listSubformats
  , exactly
  , exactlyNone
  , exactlyKnown
  , customFormat
  )
where

import           Control.DeepSeq
import           Data.Foldable                  ( foldl' )
import           Data.Generics                  ( Data
                                                , Typeable
                                                )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( not
                                                , any
                                                , or
                                                , and
                                                )

-- | An enumeration of the formats that Pandoc can recognize in a
-- 'Text.Pandoc.Definition.RawBlock' or
-- 'Text.Pandoc.Definition.RawInline', together with the string that
-- specifies them in a raw attribute. Raw elements with particular
-- 'KnownFormat's will be included by writers targeting those
-- formats. Inclusions will also happen if the 'Format' corresponds to
-- an output format that exists to specify default extensions but is
-- otherwise identical to another format, so 'AsciiDoc' will be
-- included in @asciidoctor@ output, for example. Finally, inclusions
-- can happen at other times that are discussed in [the
-- manual](https://pandoc.org/MANUAL.html#generic-raw-attribute).
--
-- The "sub-format" relationships also apply, so a 'TeX' raw element
-- will be included in any 'ConTeXt', 'LaTeX', or 'Beamer' output. See
-- 'castsToKnown' and related functions for details.
data KnownFormat
  = AsciiDoc -- ^ asciidoc
  | Beamer -- ^ beamer
  | CommonMark -- ^ commonmark
  | ConTeXt -- ^ context
  | DocBook -- ^ docbook
  | DocBook4 -- ^ docbook4
  | DocBook5 -- ^ docbook5
  | DokuWiki -- ^ dokuwiki
  | Dzslides -- ^ dzslides
  | Epub -- ^ epub
  | Epub2 -- ^ epub2
  | Epub3 -- ^ epub3
  | FictionBook -- ^ fb2
  | Haddock -- ^ haddock
  | Html -- ^ html
  | Html4 -- ^ html4
  | Html5 -- ^ html5
  | Icml -- ^ icml
  | Jats -- ^ jats
  | JatsArchiving -- ^ jats_archiving
  | JatsArticleAuthoring -- ^ jats_articleauthoring
  | JatsPublishing -- ^ jats_publishing
  | Jira -- ^ jira
  | LaTeX -- ^ latex
  | Man -- ^ man
  | Markdown -- ^ markdown
  | MediaWiki -- ^ mediawiki
  | Ms -- ^ ms
  | Muse -- ^ muse
  | OpenDocument -- ^ opendocument
  | OpenXml -- ^ openxml
  | Org -- ^ org
  | RevealJS -- ^ revealjs
  | Rst -- ^ rst
  | Rtf -- ^ rtf
  | S5 -- ^ s5
  | Slideous -- ^ slideous
  | Slidy -- ^ slidy
  | TeX -- ^ tex
  | Tei -- ^ tei
  | Texinfo -- ^ texinfo
  | Textile -- ^ textile
  | XWiki -- ^ xwiki
  | ZimWiki -- ^ zimwiki
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | A 'Format' that can be included in a
-- 'Text.Pandoc.Definition.RawBlock' or
-- 'Text.Pandoc.Definition.RawInline', or used in a 'Formats' pattern
-- to control conditional rendering in an
-- 'Text.Pandoc.Definition.IfFormatBlock' or
-- 'Text.Pandoc.Definition.IfFormatInline'. Note that 'CustomFormat'
-- will never be rendered by the Pandoc writers: it is there for
-- internal filter use.
data Format = KnownFormat KnownFormat | CustomFormat Text
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Attempt to identify the specifying string of a known
-- format. Ignores case.
--
-- > toKnownFormat "asciidoc" = Just AsciiDoc
-- > toKnownFormat "jats_archiving" = Just JatsArchiving
-- > toKnownFormat "latex" = Just LaTeX
-- > toKnownFormat "lAtEx" = Just LaTeX
-- > toKnownFormat "custom" = Nothing
toKnownFormat :: Text -> Maybe KnownFormat
toKnownFormat = flip Map.lookup m . Text.toLower
 where
  m = Map.fromList
    [ ("asciidoc"             , AsciiDoc)
    , ("beamer"               , Beamer)
    , ("commonmark"           , CommonMark)
    , ("context"              , ConTeXt)
    , ("docbook"              , DocBook)
    , ("docbook4"             , DocBook4)
    , ("docbook5"             , DocBook5)
    , ("dokuwiki"             , DokuWiki)
    , ("dzslides"             , Dzslides)
    , ("epub"                 , Epub)
    , ("epub2"                , Epub2)
    , ("epub3"                , Epub3)
    , ("fb2"                  , FictionBook)
    , ("haddock"              , Haddock)
    , ("html"                 , Html)
    , ("html4"                , Html4)
    , ("html5"                , Html5)
    , ("icml"                 , Icml)
    , ("jats"                 , Jats)
    , ("jats_archiving"       , JatsArchiving)
    , ("jats_articleauthoring", JatsArticleAuthoring)
    , ("jats_publishing"      , JatsPublishing)
    , ("jira"                 , Jira)
    , ("latex"                , LaTeX)
    , ("man"                  , Man)
    , ("markdown"             , Markdown)
    , ("mediawiki"            , MediaWiki)
    , ("ms"                   , Ms)
    , ("muse"                 , Muse)
    , ("opendocument"         , OpenDocument)
    , ("openxml"              , OpenXml)
    , ("org"                  , Org)
    , ("revealjs"             , RevealJS)
    , ("rst"                  , Rst)
    , ("rtf"                  , Rtf)
    , ("s5"                   , S5)
    , ("slideous"             , Slideous)
    , ("slidy"                , Slidy)
    , ("tex"                  , TeX)
    , ("tei"                  , Tei)
    , ("texinfo"              , Texinfo)
    , ("textile"              , Textile)
    , ("xwiki"                , XWiki)
    , ("zimwiki"              , ZimWiki)
    ]

-- | Convert a known format to the string that specifies it as a
-- format.
--
-- > fromKnownFormat AsciiDoc = "asciidoc"
-- > fromKnownFormat JatsArchiving = "jats_archiving"
fromKnownFormat :: KnownFormat -> Text
fromKnownFormat AsciiDoc             = "asciidoc"
fromKnownFormat Beamer               = "beamer"
fromKnownFormat CommonMark           = "commonmark"
fromKnownFormat ConTeXt              = "context"
fromKnownFormat DocBook              = "docbook"
fromKnownFormat DocBook4             = "docbook4"
fromKnownFormat DocBook5             = "docbook5"
fromKnownFormat DokuWiki             = "dokuwiki"
fromKnownFormat Dzslides             = "dzslides"
fromKnownFormat Epub                 = "epub"
fromKnownFormat Epub2                = "epub2"
fromKnownFormat Epub3                = "epub3"
fromKnownFormat FictionBook          = "fb2"
fromKnownFormat Haddock              = "haddock"
fromKnownFormat Html                 = "html"
fromKnownFormat Html4                = "html4"
fromKnownFormat Html5                = "html5"
fromKnownFormat Icml                 = "icml"
fromKnownFormat Jats                 = "jats"
fromKnownFormat JatsArchiving        = "jats_archiving"
fromKnownFormat JatsArticleAuthoring = "jats_articleauthoring"
fromKnownFormat JatsPublishing       = "jats_publishing"
fromKnownFormat Jira                 = "jira"
fromKnownFormat LaTeX                = "latex"
fromKnownFormat Man                  = "man"
fromKnownFormat Markdown             = "markdown"
fromKnownFormat MediaWiki            = "mediawiki"
fromKnownFormat Ms                   = "ms"
fromKnownFormat Muse                 = "muse"
fromKnownFormat OpenDocument         = "opendocument"
fromKnownFormat OpenXml              = "openxml"
fromKnownFormat Org                  = "org"
fromKnownFormat RevealJS             = "revealjs"
fromKnownFormat Rst                  = "rst"
fromKnownFormat Rtf                  = "rtf"
fromKnownFormat S5                   = "s5"
fromKnownFormat Slideous             = "slideous"
fromKnownFormat Slidy                = "slidy"
fromKnownFormat TeX                  = "tex"
fromKnownFormat Tei                  = "tei"
fromKnownFormat Texinfo              = "texinfo"
fromKnownFormat Textile              = "textile"
fromKnownFormat XWiki                = "xwiki"
fromKnownFormat ZimWiki              = "zimwiki"

-- | The expression @x \`castsTo\` f@ is @True@ when a format @x@ can
-- always be included, without modification, in a context of format
-- @f@. The 'castsTo' function is defined as
--
-- > x `castsTo` y = y `matches` x
--
-- so it is True whenever @y@ is a sub-format of @x@. See 'ToFormats',
-- 'matches', and 'listSubformats' for details. This definition means
-- that 'castsTo' is a reflexive and transitive relation.
castsTo :: Format -> Format -> Bool
castsTo x y = y `matches` x

-- | 'castsToKnown' is a tool for writers to learn about a raw element
-- of unknown format @x@ (see also 'castsTo'). For instance, an
-- 'Html5' writer can know if it can include a raw element with format
-- @x@ verbatim in its output with
--
-- > x `castsToKnown` Html5  -- True for x in [Html, Html5]
--
-- and it can know if the element is supported math content with
--
-- > x `castsToKnown` LaTeX -- True for x in [TeX, LaTeX]
--
-- This function is defined as
--
-- @
-- 'castsToKnown' f = 'castsTo' f . 'KnownFormat'
-- @
castsToKnown :: Format -> KnownFormat -> Bool
castsToKnown f = castsTo f . KnownFormat

-- $formats
-- The 'Formats' type expresses patterns that a particular 'Format'
-- can be matched against. The idea is that if a writer is targeting a
-- particular concrete 'KnownFormat' @f@ (like 'LaTeX' or 'Html5') and
-- sees an @'IfFormatBlock' p content@ (or an 'IfFormatLine'), it will
-- render @content@ when @'knownMatches' f p@. Note that the format
-- @f@ can be an intermediate format, not necessarily a format that
-- corresponds to the one specified by @--to@: the @epub3@ writer
-- considers itself to be 'Html5', and the @pdf@ writer will have
-- whatever @engine@ is used as its format.
--
-- These patterns are intended to capture the fuzziness and sub-format
-- relationships between the different elements of 'KnownFormat'. This is
-- accomplished by the 'Formats' functions, which take @'ToFormats'@
-- arguments that can interpret a particular 'KnownFormat' as representing
-- multiple 'Format's. So we have
--
-- > f `knownMatches` TeX      -- True when f is [TeX, LaTeX, ConTeXt, Beamer]
-- > f `knownMatches` LaTeX    -- True when f is [TeX, LaTeX, Beamer]
-- > f `knownMatches` Html5    -- True when f is [Dzslides, Epub3, Html5, RevealJS]
--
-- and the Boolean algebra operations on 'Formats' can construct more
-- general patterns. Some concrete examples:
--
-- > TeX `knownMatches` TeX = True
-- > TeX `knownMatches` LaTeX = False
-- > LaTeX `knownMatches` TeX = True
-- > LaTeX `knownMatches` LaTeX = True
-- > LaTeX `knownMatches` Beamer = False
-- > LaTeX `knownMatches` (LaTeX `or` Html) = True
-- > LaTeX `knownMatches` (TeX `except` Beamer) = True
-- > LaTeX `knownMatches` (TeX `except` Markdown) = True
-- > LaTeX `knownMatches` (LaTeX `and` Html) = False
-- > LaTeX `knownMatches` not Html = True
--
-- A 'CustomFormat' in a pattern in 'IfFormatBlock' or
-- 'IfFormatInline' will never make a difference in the Pandoc
-- writers, since they will only match against a 'KnownFormat'.


-- | A 'Formats' value is a pattern that a particular 'Format' can be
-- matched against using 'matches' or 'knownMatches'.
data Formats
  = OneOfFormats (Set Format) -- ^ match one of the given formats
  | NoneOfFormats (Set Format) -- ^ match anything but one of the given formats
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | The class of types that can be interpreted as a 'Formats'
-- pattern. The instance for 'KnownFormat' is "fuzzy", in that a
-- 'KnownFormat' will become a pattern that matches against multiple
-- formats (see 'listSubformats'). So, for instance,
--
-- @
-- 'toFormats' 'TeX' = 'exactlyKnown' ['Beamer', 'ConTeXt', 'LaTeX', 'TeX']
-- @
--
-- and for a 'KnownFormat' in general,
--
-- @
-- 'toFormats' = 'listSubformats'
-- @
--
-- Note that the instance for 'Formats' is simply @'toFormats' = id@,
-- i.e. a 'Formats' pattern is never itself interpreted fuzzily. This
-- also holds for the 'CustomFormat' branch of 'Format'.
class ToFormats a where
  toFormats :: a -> Formats

instance ToFormats Formats where
  toFormats = id

instance ToFormats KnownFormat where
  toFormats = listSubformats

instance ToFormats Format where
  toFormats (KnownFormat f)  = listSubformats f
  toFormats x@CustomFormat{} = exactly [x]

-- | 'anything' matches any format.
anything :: Formats
anything = NoneOfFormats mempty

-- | 'nothing' matches no format. Note that @nothing = mempty@.
nothing :: Formats
nothing = OneOfFormats mempty

-- | 'anyOf' matches any of the given patterns.
anyOf :: (Foldable t, ToFormats a) => t a -> Formats
anyOf = foldl' go nothing where go x y = x `or` toFormats y

-- | 'noneOf' matches anything except the given patterns.
noneOf :: (Foldable t, ToFormats a) => t a -> Formats
noneOf = foldl' go anything where go x y = x `except` toFormats y

-- | 'exactly' matches exactly the given formats. You will normally
-- want to use the functions accepting 'ToFormats' types to construct
-- a 'Formats' pattern instead of using this directly.
exactly :: [Format] -> Formats
exactly = OneOfFormats . Set.fromList

-- | 'customFormat' matches exactly the given custom format. Note that
-- a 'CustomFormat' in a 'Formats' pattern will generally make no
-- difference to matching in the Pandoc writers, since they do not
-- know about any 'CustomFormat's, but this may be useful in filters.
customFormat :: Text -> Formats
customFormat = OneOfFormats . Set.singleton . CustomFormat

-- | 'exactlyNone' matches anything except exactly the given
-- formats. See 'exactly'.
exactlyNone :: [Format] -> Formats
exactlyNone = NoneOfFormats . Set.fromList

-- | 'exactlyKnown' matches exactly the given known formats. See
-- 'exactly'.
exactlyKnown :: [KnownFormat] -> Formats
exactlyKnown = exactly . fmap KnownFormat

-- | @'and' x y@ matches a format @f@ when @f@ matches both @x@ and
-- @y@.
and :: (ToFormats a, ToFormats b) => a -> b -> Formats
and x = and' (toFormats x) . toFormats
 where
  and' (OneOfFormats s) (OneOfFormats t) =
    OneOfFormats $ s `Set.intersection` t
  and' (OneOfFormats  s) (NoneOfFormats t) = OneOfFormats $ s `Set.difference` t
  and' (NoneOfFormats s) (OneOfFormats  t) = OneOfFormats $ t `Set.difference` s
  and' (NoneOfFormats s) (NoneOfFormats t) = NoneOfFormats $ s `Set.union` t


-- | @'or' x y@ matches a format @f@ when @f@ matches either @x@ or
-- @y@.
or :: (ToFormats a, ToFormats b) => a -> b -> Formats
or x = not . and (not x) . not

-- | @'not' x@ matches a format @f@ when @f@ does not match @x@.
not :: (ToFormats a) => a -> Formats
not = not' . toFormats
 where
  not' (OneOfFormats  s) = NoneOfFormats s
  not' (NoneOfFormats s) = OneOfFormats s

-- | @'except' x y@ matches a format @f@ when @f@ matches @x@ and
-- not @y@.
--
-- > x `except` y = x `and` not y
except :: (ToFormats a, ToFormats b) => a -> b -> Formats
except x = and x . not

-- | Test if the given 'Format' matches against a pattern. This
-- function should be used to test for conditional rendering (seeing
-- if a concrete 'Format' matches a 'Formats' pattern), not for raw
-- element inclusion. For the latter, use 'castsTo'.
--
-- The 'matches' relation is reflexive (@x \`matches\` x@ is always
-- @True@) and transitive (if @x \`matches\` y@ and @y \`matches\` z@
-- are @True@, then @x \`matches\` z@ is @True@).
matches :: ToFormats a => Format -> a -> Bool
matches f p = case toFormats p of
  OneOfFormats  s -> f `Set.member` s
  NoneOfFormats s -> f `Set.notMember` s

-- | Test if the given 'KnownFormat' matches against a pattern. See
-- 'matches'.
--
-- > knownMatches = matches . KnownFormat
knownMatches :: ToFormats a => KnownFormat -> a -> Bool
knownMatches = matches . KnownFormat

-- | The 'listSubformats' function defines how other formats will
-- match against the given 'Format', considered as a 'Formats'
-- pattern. It is intended to capture a sub-format/super-format
-- relation on 'Format'. You should not need to use this function, as
-- the relevant 'Formats' functions take 'ToFormats' arguments.
--
-- A general guideline is that if @y@ matches @'listSubformats' x@,
-- then whenever a raw element of format @x@ can be included in a
-- document, a raw element of format @y@ can be included in the same
-- place and in the same way. It is also generally true that @x@ will
-- be a subset of @y@, or will be based on @y@ in some way. So
-- @'listSubformats' y@ lists all the formats not-more-general than
-- @y@ in that sense (the sub-formats or derived formats of @y@).
--
-- Some examples:
--
-- @
-- 'listSubformats' 'TeX'     = 'exactlyKnown' ['Beamer', 'ConTeXt', 'LaTeX', 'TeX']
-- 'listSubformats' 'LaTeX'   = 'exactlyKnown' ['LaTeX', 'Beamer']
-- 'listSubformats' 'ConTeXt' = 'exactlyKnown' ['ConTeXt']
-- 'listSubformats' 'Html4'   = 'exactlyKnown' ['Epub2', 'Html4', 'S5', 'Slideous', 'Slidy']
-- @
listSubformats :: KnownFormat -> Formats
listSubformats AsciiDoc    = exactlyKnown [AsciiDoc]
listSubformats Beamer      = exactlyKnown [Beamer]
listSubformats CommonMark  = exactlyKnown [CommonMark, Markdown]
listSubformats ConTeXt     = exactlyKnown [ConTeXt]
listSubformats DocBook     = exactlyKnown [DocBook, DocBook4, DocBook5]
listSubformats DocBook4    = exactlyKnown [DocBook4]
listSubformats DocBook5    = exactlyKnown [DocBook5]
listSubformats Dzslides    = exactlyKnown [Dzslides]
listSubformats DokuWiki    = exactlyKnown [DokuWiki]
listSubformats Epub        = exactlyKnown [Epub, Epub2, Epub3]
listSubformats Epub2       = exactlyKnown [Epub2]
listSubformats Epub3       = exactlyKnown [Epub3]
listSubformats FictionBook = exactlyKnown [FictionBook]
listSubformats Haddock     = exactlyKnown [Haddock]
listSubformats Html        = exactlyKnown
  [ Dzslides
  , Epub
  , Epub2
  , Epub3
  , Html
  , Html4
  , Html5
  , RevealJS
  , S5
  , Slideous
  , Slidy
  , Textile
  ]
listSubformats Html4 = exactlyKnown [Epub2, Html4, S5, Slideous, Slidy]
listSubformats Html5 = exactlyKnown [Dzslides, Epub3, Html5, RevealJS]
listSubformats Icml  = exactlyKnown [Icml]
listSubformats Jats =
  exactlyKnown [Jats, JatsArchiving, JatsArticleAuthoring, JatsPublishing]
listSubformats JatsArchiving = exactlyKnown [JatsArchiving]
listSubformats JatsArticleAuthoring =
  exactlyKnown [JatsArchiving, JatsArticleAuthoring, JatsPublishing]
listSubformats JatsPublishing = exactlyKnown [JatsArchiving, JatsPublishing]
listSubformats Jira           = exactlyKnown [Jira]
listSubformats LaTeX          = exactlyKnown [Beamer, LaTeX]
listSubformats Man            = exactlyKnown [Man]
listSubformats Markdown       = exactlyKnown [Markdown]
listSubformats MediaWiki      = exactlyKnown [MediaWiki]
listSubformats Ms             = exactlyKnown [Ms]
listSubformats Muse           = exactlyKnown [Muse]
listSubformats OpenDocument   = exactlyKnown [OpenDocument]
listSubformats OpenXml        = exactlyKnown [OpenXml]
listSubformats Org            = exactlyKnown [Org]
listSubformats RevealJS       = exactlyKnown [RevealJS]
listSubformats Rst            = exactlyKnown [Rst]
listSubformats Rtf            = exactlyKnown [Rtf]
listSubformats S5             = exactlyKnown [S5]
listSubformats Slideous       = exactlyKnown [Slideous]
listSubformats Slidy          = exactlyKnown [Slidy]
listSubformats TeX            = exactlyKnown [Beamer, ConTeXt, LaTeX, TeX]
listSubformats Tei            = exactlyKnown [Tei]
listSubformats Texinfo        = exactlyKnown [Texinfo]
listSubformats Textile        = exactlyKnown [Textile]
listSubformats XWiki          = exactlyKnown [XWiki]
listSubformats ZimWiki        = exactlyKnown [ZimWiki]

instance NFData KnownFormat
instance NFData Format
instance NFData Formats
