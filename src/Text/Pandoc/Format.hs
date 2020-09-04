{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
   Module      : Text.Pandoc.Format
   Copyright   : Copyright 2020 Christian Despres
   License     : BSD3

   Maintainer  : Christian Despres <christian.j.j.despres@gmail.com>
   Stability   : alpha
   Portability : portable

Definition of a 'Format' type that enumerates the formats that Pandoc
recognizes, and a 'Formats' type and combinators to implement 'Format'
patterns and matching. This module was designed to have its functions
imported qualified.
-}

module Text.Pandoc.Format
  ( -- * The 'Format' type
    Format(..)
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

  -- * Direct 'Formats' conversions
  , listSubformats
  , exactly
  , toList
  )
where

import           Control.DeepSeq
import           Data.Foldable                  ( foldl' )
import           Data.Generics                  ( Data
                                                , Typeable
                                                )
import           Data.Semigroup                 ( Semigroup(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( not
                                                , any
                                                , or
                                                , and
                                                )

-- | An enumeration of the formats that Pandoc can recognize in some
-- way. The formats are listed with the string that will specify them
-- in input, output, or as the attribute of a
-- 'Text.Pandoc.Definition.RawBlock' or
-- 'Text.Pandoc.Definition.RawInline' (when applicable).
data Format
  = AsciiDoc -- ^ asciidoc
  | AsciiDoctor -- ^ asciidoctor
  | Beamer -- ^ beamer
  | CommonMark -- ^ commonmark
  | CommonMarkX -- ^ commonmark_x
  | ConTeXt -- ^ context
  | Creole -- ^ creole
  | Csv -- ^ csv
  | DocBook -- ^ docbook
  | DocBook4 -- ^ docbook4
  | DocBook5 -- ^ docbook5
  | Docx -- ^ docx
  | DokuWiki -- ^ dokuwiki
  | Dzslides -- ^ dzslides
  | Epub -- ^ epub
  | Epub2 -- ^ epub2
  | Epub3 -- ^ epub3
  | FictionBook -- ^ fb2
  | Gfm -- ^ gfm
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
  | Json -- ^ json
  | Jupyter -- ^ ipynb
  | LaTeX -- ^ latex
  | Man -- ^ man
  | Markdown -- ^ markdown
  | MarkdownGithub -- ^ markdown_github (deprecated)
  | MarkdownMmd -- ^ markdown_mmd
  | MarkdownPhpExtra -- ^ markdown_phpextra
  | MarkdownStrict -- ^ markdown_strict
  | MediaWiki -- ^ mediawiki
  | Ms -- ^ ms
  | Muse -- ^ muse
  | Native -- ^ native
  | Odt -- ^ odt
  | OpenDocument -- ^ opendocument
  | OpenXml -- ^ openxml
  | Opml -- ^ opml
  | Org -- ^ org
  | Pdf -- ^ pdf
  | Plain -- ^ plain
  | Pptx -- ^ pptx
  | RevealJS -- ^ revealjs
  | Rst -- ^ rst
  | Rtf -- ^ rtf
  | S5 -- ^ s5
  | Slideous -- ^ slideous
  | Slidy -- ^ slidy
  | T2T -- ^ t2t
  | TWiki -- ^ twiki
  | TeX -- ^ tex
  | Tei -- ^ tei
  | Texinfo -- ^ texinfo
  | Textile -- ^ textile
  | TikiWiki -- ^ tikiwiki
  | VimWiki -- ^ vimwiki
  | XWiki -- ^ xwiki
  | ZimWiki -- ^ zimwiki
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | The expression @x \`castsTo\` f@ is @True@ when a format @x@ can
-- always be included, without modification, in a context of format
-- @f@. It can used by writers to learn more about a raw element of
-- unknown format @x@. For instance, an 'Html5' writer can know if it
-- can include a raw element with format @x@ verbatim in its output
-- with
--
-- > x `castsTo` Html5  -- True for x in [Html, Html5]
--
-- and it can know if the element is supported math content with
--
-- > x `castsTo` LaTeX -- True for x in [TeX, LaTeX]
--
-- The 'castsTo' function is defined as
--
-- > x `castsTo` y = y `matches` x
--
-- so it is True whenever @y@ is a sub-format of @x@. See 'ToFormats',
-- 'matches', and 'listSubformats' for details. This definition means
-- that 'castsTo' is a reflexive and transitive relation.
castsTo :: Format -> Format -> Bool
castsTo x y = y `matches` x

-- $formats
-- The 'Formats' type expresses patterns that a particular 'Format'
-- can be matched against. The idea is that if a writer is targeting a
-- particular concrete 'Format' @f@ (like 'LaTeX' or 'Html5') and sees
-- an @'IfFormatBlock' p content@ (or an 'IfFormatLine'), it will
-- render @content@ when @'matches' f p@. Note that the format @f@ can
-- be an intermediate format, not necessarily the format specified by
-- @--to@: the @epub3@ writer considers itself to be 'Html5', and the
-- @pdf@ writer will have whatever @engine@ is used as its format.
--
-- These patterns are intended to capture the fuzziness and sub-format
-- relationships between the different elements of 'Format'. This is
-- accomplished by the 'Formats' functions, which take @'ToFormats'@
-- arguments that can interpret a particular 'Format' as representing
-- multiple 'Format's. So we have
--
-- > f `matches` TeX      -- True when f is [TeX, LaTeX, ConTeXt, Beamer]
-- > f `matches` LaTeX    -- True when f is [TeX, LaTeX, Beamer]
-- > f `matches` Markdown -- True when f is any Markdown
--
-- but the Boolean algebra operations on 'Formats' can construct more
-- general patterns. Some concrete examples:
--
-- > TeX `matches` TeX = True
-- > TeX `matches` LaTeX = False
-- > LaTeX `matches` TeX = True
-- > LaTeX `matches` LaTeX = True
-- > LaTeX `matches` Beamer = False
-- > LaTeX `matches` (LaTeX `or` Html) = True
-- > LaTeX `matches` (TeX `except` Beamer) = True
-- > LaTeX `matches` (TeX `except` Markdown) = True
-- > LaTeX `matches` (LaTeX `and` Html) = False
-- > LaTeX `matches` not Html = True

-- | A 'Formats' value is a pattern that a particular 'Format' can be
-- matched against using 'matches'. At a lower level, the expression
-- @Formats x@ means "any of the formats in @x@".
--
-- The 'Monoid' instance has @mempty = 'nothing'@ and @(<>) = 'or'@.
newtype Formats = Formats (Set Format)
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, Semigroup, Monoid)

-- | The class of types that can be interpreted as a 'Formats'
-- pattern. The instance for 'Format' is "fuzzy", in that a 'Format'
-- will become a pattern that matches against multiple formats (see
-- 'listSubformats'). So, for instance,
--
-- @
-- 'toFormats' 'TeX' = 'exactly' ['Beamer', 'ConTeXt', 'LaTeX', 'TeX']
-- @
--
-- and for a 'Format' in general,
--
-- @
-- 'toFormats' = 'listSubformats'
-- @
--
-- Note that the instance for 'Formats' is simply @'toFormats' = id@,
-- i.e. a 'Formats' pattern is never itself interpreted fuzzily.
class ToFormats a where
  toFormats :: a -> Formats

instance ToFormats Formats where
  toFormats = id

instance ToFormats Format where
  toFormats = listSubformats

-- | 'anything' matches any format.
anything :: Formats
anything = exactly [minBound .. maxBound]

-- | 'nothing' matches no format. Note that @nothing = mempty@.
nothing :: Formats
nothing = mempty

-- | 'anyOf' matches any of the given patterns.
anyOf :: (Foldable t, ToFormats a) => t a -> Formats
anyOf = foldl' go mempty where go x y = x `or` toFormats y

-- | 'noneOf' matches anything except the given patterns.
noneOf :: (Foldable t, ToFormats a) => t a -> Formats
noneOf = foldl' go anything where go x y = x `except` toFormats y

-- | 'exactly' matches exactly the given formats. You will normally
-- want to use the functions accepting 'ToFormats' types to construct
-- a 'Formats' pattern instead of using this directly.
exactly :: [Format] -> Formats
exactly = Formats . Set.fromList

-- | @'and' x y@ matches a format @f@ when @f@ matches both @x@ and
-- @y@.
and :: (ToFormats a, ToFormats b) => a -> b -> Formats
and x y = Formats $ s `Set.intersection` t
 where
  Formats s = toFormats x
  Formats t = toFormats y

-- | @'or' x y@ matches a format @f@ when @f@ matches either @x@ or
-- @y@.
or :: (ToFormats a, ToFormats b) => a -> b -> Formats
or x y = Formats $ s `Set.union` t
 where
  Formats s = toFormats x
  Formats t = toFormats y

-- | @'not' x@ matches a format @f@ when @f@ does not match @x@.
not :: (ToFormats a) => a -> Formats
not t = anything `except` (toFormats t)

-- | @'except' x y@ matches a format @f@ when @f@ matches @x@ and
-- not @y@.
--
-- > x `except` y = x `and` not y
except :: (ToFormats a, ToFormats b) => a -> b -> Formats
except x y = Formats $ s `Set.difference` t
 where
  Formats s = toFormats x
  Formats t = toFormats y

-- | Test if the given 'Format' matches against a pattern. This
-- function should be used to test for conditional rendering (seeing
-- if a concrete 'Format' matches a 'Formats' pattern), not for raw
-- element inclusion. For the latter, use 'castsTo'.
--
-- The 'matches' relation is reflexive (@x \`matches\` x@ is always
-- @True@) and transitive (if @x \`matches\` y@ and @y \`matches\` z@
-- are @True@, then @x \`matches\` z@ is @True@).
matches :: ToFormats a => Format -> a -> Bool
matches f p = f `Set.member` s where Formats s = toFormats p

-- | The 'listSubformats' function defines how other formats will
-- match against the given 'Format', considered as a 'Formats'
-- pattern. It is intended to capture a sub-format/super-format
-- relation on 'Format'.
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
-- 'listSubformats' 'TeX'      = 'exactly' ['Beamer', 'ConTeXt', 'LaTeX', 'TeX']
-- 'listSubformats' 'LaTeX'    = 'exactly' ['LaTeX', 'Beamer']
-- 'listSubformats' 'ConTeXt'  = 'exactly' ['ConTeXt']
-- 'listSubformats' 'Epub'     = 'exactly' ['Epub', 'Epub2', 'Epub3']
-- 'listSubformats' 'Markdown' = 'exactly'
--   ['Markdown', 'MarkdownGithub', 'MarkdownMmd', 'MarkdownPhpExtra', 'MarkdownStrict']
-- @
listSubformats :: Format -> Formats
listSubformats AsciiDoc             = exactly [AsciiDoc]
listSubformats AsciiDoctor          = exactly [AsciiDoctor]
listSubformats Beamer               = exactly [Beamer]
listSubformats CommonMark           = exactly [CommonMark]
listSubformats CommonMarkX          = exactly [CommonMarkX]
listSubformats ConTeXt              = exactly [ConTeXt]
listSubformats Creole               = exactly [Creole]
listSubformats Csv                  = exactly [Csv]
listSubformats DocBook              = exactly [DocBook]
listSubformats DocBook4             = exactly [DocBook4]
listSubformats DocBook5             = exactly [DocBook5]
listSubformats Docx                 = exactly [Docx]
listSubformats DokuWiki             = exactly [DokuWiki]
listSubformats Dzslides             = exactly [Dzslides]
listSubformats Epub                 = exactly [Epub, Epub2, Epub3]
listSubformats Epub2                = exactly [Epub2]
listSubformats Epub3                = exactly [Epub3]
listSubformats FictionBook          = exactly [FictionBook]
listSubformats Gfm                  = exactly [Gfm]
listSubformats Haddock              = exactly [Haddock]
listSubformats Html = exactly [Epub, Epub2, Epub3, Html, Html4, Html5]
listSubformats Html4                = exactly [Epub2, Html4]
listSubformats Html5                = exactly [Epub3, Html5]
listSubformats Icml                 = exactly [Icml]
listSubformats Jats                 = exactly [Jats]
listSubformats JatsArchiving        = exactly [JatsArchiving]
listSubformats JatsArticleAuthoring = exactly [JatsArticleAuthoring]
listSubformats JatsPublishing       = exactly [JatsPublishing]
listSubformats Jira                 = exactly [Jira]
listSubformats Json                 = exactly [Json]
listSubformats Jupyter              = exactly [Jupyter]
listSubformats LaTeX                = exactly [Beamer, LaTeX]
listSubformats Man                  = exactly [Man]
listSubformats Markdown             = exactly
  [Markdown, MarkdownGithub, MarkdownMmd, MarkdownPhpExtra, MarkdownStrict]
listSubformats MarkdownGithub = exactly
  [Markdown, MarkdownGithub, MarkdownMmd, MarkdownPhpExtra, MarkdownStrict]
listSubformats MarkdownMmd = exactly
  [Markdown, MarkdownGithub, MarkdownMmd, MarkdownPhpExtra, MarkdownStrict]
listSubformats MarkdownPhpExtra = exactly
  [Markdown, MarkdownGithub, MarkdownMmd, MarkdownPhpExtra, MarkdownStrict]
listSubformats MarkdownStrict = exactly
  [Markdown, MarkdownGithub, MarkdownMmd, MarkdownPhpExtra, MarkdownStrict]
listSubformats MediaWiki    = exactly [MediaWiki]
listSubformats Ms           = exactly [Ms]
listSubformats Muse         = exactly [Muse]
listSubformats Native       = exactly [Native]
listSubformats Odt          = exactly [Odt]
listSubformats OpenDocument = exactly [OpenDocument]
listSubformats OpenXml      = exactly [OpenXml]
listSubformats Opml         = exactly [Opml]
listSubformats Org          = exactly [Org]
listSubformats Pdf          = exactly [Pdf]
listSubformats Plain        = exactly [Plain]
listSubformats Pptx         = exactly [Pptx]
listSubformats RevealJS     = exactly [RevealJS]
listSubformats Rst          = exactly [Rst]
listSubformats Rtf          = exactly [Rtf]
listSubformats S5           = exactly [S5]
listSubformats Slideous     = exactly [Slideous]
listSubformats Slidy        = exactly [Slidy]
listSubformats T2T          = exactly [T2T]
listSubformats TWiki        = exactly [TWiki]
listSubformats TeX          = exactly [Beamer, ConTeXt, LaTeX, TeX]
listSubformats Tei          = exactly [Tei]
listSubformats Texinfo      = exactly [Texinfo]
listSubformats Textile      = exactly [Textile]
listSubformats TikiWiki     = exactly [TikiWiki]
listSubformats VimWiki      = exactly [VimWiki]
listSubformats XWiki        = exactly [XWiki]
listSubformats ZimWiki      = exactly [ZimWiki]

-- | Convert a 'Formats' into its underlying list of successful
-- matches.
toList :: Formats -> [Format]
toList (Formats s) = Set.toList s

instance NFData Format
instance NFData Formats
