{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, CPP #-}

import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Builder (singleton, plain, text, simpleTable, table, emptyCell,
                            normalizeTableHead, normalizeTableBody, normalizeTableFoot,
                            emptyCaption, simpleFigureWith)
import qualified Text.Pandoc.Builder as Builder
import Data.Generics
import Data.List (tails)
import Test.HUnit (Assertion, assertEqual, assertFailure)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck (forAll, choose, Property, Arbitrary, Testable, arbitrary, Gen)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.String.QQ
import Data.ByteString.Lazy (ByteString)
import qualified Data.Monoid as Monoid


p_walk :: (Typeable a, Walkable a Pandoc)
       => (a -> a) -> Pandoc -> Bool
p_walk f d = everywhere (mkT f) d == walk f d

p_walkList :: (Typeable a, Walkable [a] Pandoc)
       => ([a] -> [a]) -> Pandoc -> Bool
p_walkList f d = everywhere (mkT f) d == walk (foldr g []) d
  where g x ys = f (x:ys)

p_query :: (Eq a, Typeable a1, Monoid a, Walkable a1 Pandoc)
        => (a1 -> a) -> Pandoc -> Bool
p_query f d = everything mappend (mempty `mkQ` f) d == query f d

p_queryList :: (Eq a, Typeable a1, Monoid a, Walkable [a1] Pandoc)
            => ([a1] -> a) -> Pandoc -> Bool
p_queryList f d = everything mappend (mempty `mkQ` f) d ==
                  query (mconcat . map f . tails) d

inlineTrans :: Inline -> Inline
inlineTrans (Str xs) = Str $ T.toUpper xs
inlineTrans (Emph xs) = Strong xs
inlineTrans x = x

inlinesTrans :: [Inline] -> [Inline]
inlinesTrans ys | all whitespaceInline ys = []
  where
    whitespaceInline Space = True
    whitespaceInline LineBreak = True
    whitespaceInline SoftBreak = True
    whitespaceInline (Str "") = True
    whitespaceInline _ = False
inlinesTrans ys = ys

blockTrans :: Block -> Block
blockTrans (Plain xs) = Para xs
blockTrans (BlockQuote xs) = Div ("",["special"],[]) xs
blockTrans x = x

blocksTrans :: [Block] -> [Block]
blocksTrans [CodeBlock {}] = []
blocksTrans [BlockQuote xs] = xs
blocksTrans [Div _ xs] = xs
blocksTrans xs = xs

inlineQuery :: Inline -> Text
inlineQuery (Str xs) = xs
inlineQuery _ = ""

inlinesQuery :: [Inline] -> Monoid.Sum Int
inlinesQuery = Monoid.Sum . length

blockQuery :: Block -> [Int]
blockQuery (Header lev _ _) = [lev]
blockQuery _ = []

blocksQuery :: [Block] -> Monoid.Sum Int
blocksQuery = Monoid.Sum . length


prop_roundtrip :: Pandoc -> Bool
prop_roundtrip doc = case decode $ encode doc :: (Maybe Pandoc) of
  Just doc' -> doc == doc'
  _         -> False

testEncode :: ToJSON a => (a, ByteString) -> Assertion
testEncode (doc, j) = assertEqual "Encoding error" (encode doc) j

testDecode' :: FromJSON a => (a, ByteString) -> Maybe a
testDecode' (_, j) = decode j

testDecode :: (Show a, Eq a, FromJSON a) => (a, ByteString) -> Assertion
testDecode (doc, j) =
  case testDecode' (doc, j) of
    Just doc' -> assertEqual "Decoding error" doc' doc
    Nothing   -> assertFailure "Decoding error"

testEncodeDecode :: (Show a, Eq a, ToJSON a, FromJSON a)
                 => String
                 -> (a, ByteString)
                 -> Test
testEncodeDecode msg pair = testGroup msg [ testCase "Encoding" $ testEncode pair
                                          , testCase "Decoding" $ testDecode pair
                                          ]

t_meta :: (Meta, ByteString)
t_meta = ( Meta $ M.fromList [("foo", MetaBool True)]
         , [s|{"foo":{"t":"MetaBool","c":true}}|]
         )

t_metamap :: (MetaValue, ByteString)
t_metamap = ( MetaMap $
              M.fromList [("foo", MetaBool True)]
            , [s|{"t":"MetaMap","c":{"foo":{"t":"MetaBool","c":true}}}|]
            )

t_metalist :: (MetaValue, ByteString)
t_metalist = ( MetaList [MetaBool True, MetaString "baz"]
             , [s|{"t":"MetaList","c":[{"t":"MetaBool","c":true},{"t":"MetaString","c":"baz"}]}|]
             )

t_metabool :: (MetaValue, ByteString)
t_metabool = ( MetaBool False, [s|{"t":"MetaBool","c":false}|] )

t_metastring :: (MetaValue, ByteString)
t_metastring = ( MetaString "Hello", [s|{"t":"MetaString","c":"Hello"}|] )

t_metainlines :: (MetaValue, ByteString)
t_metainlines = ( MetaInlines [Space, SoftBreak]
                , [s|{"t":"MetaInlines","c":[{"t":"Space"},{"t":"SoftBreak"}]}|]
                )

t_metablocks :: (MetaValue, ByteString)
t_metablocks = ( MetaBlocks [Null,Null], [s|{"t":"MetaBlocks","c":[{"t":"Null"},{"t":"Null"}]}|])

t_singlequote :: (QuoteType, ByteString)
t_singlequote = (SingleQuote, [s|{"t":"SingleQuote"}|])

t_doublequote :: (QuoteType, ByteString)
t_doublequote = (DoubleQuote, [s|{"t":"DoubleQuote"}|])

t_authorintext :: (CitationMode, ByteString)
t_authorintext = (AuthorInText, [s|{"t":"AuthorInText"}|])

t_suppressauthor :: (CitationMode, ByteString)
t_suppressauthor = (SuppressAuthor, [s|{"t":"SuppressAuthor"}|])

t_normalcitation :: (CitationMode, ByteString)
t_normalcitation = (NormalCitation, [s|{"t":"NormalCitation"}|])

t_citation :: (Citation, ByteString)
t_citation = ( Citation { citationId = "jameson:unconscious",
                          citationPrefix = [Str "cf"],
                          citationSuffix = [Space,Str "123"],
                          citationMode = NormalCitation,
                          citationNoteNum = 0,
                          citationHash = 0}
             , [s|{"citationId":"jameson:unconscious","citationPrefix":[{"t":"Str","c":"cf"}],"citationSuffix":[{"t":"Space"},{"t":"Str","c":"123"}],"citationMode":{"t":"NormalCitation"},"citationNoteNum":0,"citationHash":0}|]
             )

t_displaymath :: (MathType, ByteString)
t_displaymath = ( DisplayMath, [s|{"t":"DisplayMath"}|])

t_inlinemath :: (MathType, ByteString)
t_inlinemath = ( InlineMath, [s|{"t":"InlineMath"}|])


t_str :: (Inline, ByteString)
t_str = ( Str "Hello"
        , [s|{"t":"Str","c":"Hello"}|]
        )

t_emph :: (Inline, ByteString)
t_emph = ( Emph [Str "Hello"]
         , [s|{"t":"Emph","c":[{"t":"Str","c":"Hello"}]}|]
         )

t_underline :: (Inline, ByteString)
t_underline = ( Underline [Str "Hello"]
         , [s|{"t":"Underline","c":[{"t":"Str","c":"Hello"}]}|]
         )

t_strong :: (Inline, ByteString)
t_strong = ( Strong [Str "Hello"]
           , [s|{"t":"Strong","c":[{"t":"Str","c":"Hello"}]}|]
           )

t_strikeout :: (Inline, ByteString)
t_strikeout = ( Strikeout [Str "Hello"]
              , [s|{"t":"Strikeout","c":[{"t":"Str","c":"Hello"}]}|]
              )

t_superscript :: (Inline, ByteString)
t_superscript = ( Superscript [Str "Hello"]
                , [s|{"t":"Superscript","c":[{"t":"Str","c":"Hello"}]}|]
                )

t_subscript :: (Inline, ByteString)
t_subscript = ( Subscript [Str "Hello"]
              , [s|{"t":"Subscript","c":[{"t":"Str","c":"Hello"}]}|]
              )

t_smallcaps :: (Inline, ByteString)
t_smallcaps = ( SmallCaps [Str "Hello"]
              , [s|{"t":"SmallCaps","c":[{"t":"Str","c":"Hello"}]}|]
              )

t_quoted :: (Inline, ByteString)
t_quoted = ( Quoted SingleQuote [Str "Hello"]
           , [s|{"t":"Quoted","c":[{"t":"SingleQuote"},[{"t":"Str","c":"Hello"}]]}|]
           )

t_cite :: (Inline, ByteString)
t_cite = ( Cite [Citation { citationId = "jameson:unconscious"
                          , citationPrefix = [Str "cf"]
                          , citationSuffix = [Space,Str "12"]
                          , citationMode = NormalCitation
                          , citationNoteNum = 0
                          , citationHash = 0}]
                [ Str "[cf"
                , Space
                , Str "@jameson:unconscious"
                , Space
                , Str "12]"]
         ,[s|{"t":"Cite","c":[[{"citationId":"jameson:unconscious","citationPrefix":[{"t":"Str","c":"cf"}],"citationSuffix":[{"t":"Space"},{"t":"Str","c":"12"}],"citationMode":{"t":"NormalCitation"},"citationNoteNum":0,"citationHash":0}],[{"t":"Str","c":"[cf"},{"t":"Space"},{"t":"Str","c":"@jameson:unconscious"},{"t":"Space"},{"t":"Str","c":"12]"}]]}|]
             )

t_code :: (Inline, ByteString)
t_code = ( Code ("", [], [("language", "haskell")]) "foo bar"
         , [s|{"t":"Code","c":[["",[],[["language","haskell"]]],"foo bar"]}|]
         )

t_space :: (Inline, ByteString)
t_space = ( Space, [s|{"t":"Space"}|] )

t_softbreak :: (Inline, ByteString)
t_softbreak = ( SoftBreak, [s|{"t":"SoftBreak"}|] )

t_linebreak :: (Inline, ByteString)
t_linebreak = ( LineBreak, [s|{"t":"LineBreak"}|] )

t_rawinline :: (Inline, ByteString)
t_rawinline = ( RawInline (Format "tex") "\\foo{bar}"
              , [s|{"t":"RawInline","c":["tex","\\foo{bar}"]}|]
              )

t_link :: (Inline, ByteString)
t_link = ( Link ("id",["kls"],[("k1", "v1"), ("k2", "v2")])
           [ Str "a", Space, Str "famous", Space, Str "site"]
           ("https://www.google.com","google")
         , [s|{"t":"Link","c":[["id",["kls"],[["k1","v1"],["k2","v2"]]],[{"t":"Str","c":"a"},{"t":"Space"},{"t":"Str","c":"famous"},{"t":"Space"},{"t":"Str","c":"site"}],["https://www.google.com","google"]]}|]
         )

t_image :: (Inline, ByteString)
t_image = ( Image ("id",["kls"],[("k1", "v1"), ("k2", "v2")])
           [ Str "a", Space, Str "famous", Space, Str "image"]
           ("my_img.png","image")
         , [s|{"t":"Image","c":[["id",["kls"],[["k1","v1"],["k2","v2"]]],[{"t":"Str","c":"a"},{"t":"Space"},{"t":"Str","c":"famous"},{"t":"Space"},{"t":"Str","c":"image"}],["my_img.png","image"]]}|]
         )

t_note :: (Inline, ByteString)
t_note = ( Note [Para [Str "Hello"]]
         , [s|{"t":"Note","c":[{"t":"Para","c":[{"t":"Str","c":"Hello"}]}]}|]
         )

t_span :: (Inline, ByteString)
t_span = ( Span ("id", ["kls"], [("k1", "v1"), ("k2", "v2")]) [Str "Hello"]
         , [s|{"t":"Span","c":[["id",["kls"],[["k1","v1"],["k2","v2"]]],[{"t":"Str","c":"Hello"}]]}|]
         )

t_plain :: (Block, ByteString)
t_plain = ( Plain [Str "Hello"]
          , [s|{"t":"Plain","c":[{"t":"Str","c":"Hello"}]}|]
          )

t_para :: (Block, ByteString)
t_para = ( Para [Str "Hello"]
          , [s|{"t":"Para","c":[{"t":"Str","c":"Hello"}]}|]
          )

t_lineblock :: (Block, ByteString)
t_lineblock = ( LineBlock [[Str "Hello"], [Str "Moin"]]
              , [s|{"t":"LineBlock","c":[[{"t":"Str","c":"Hello"}],[{"t":"Str","c":"Moin"}]]}|]
              )

t_codeblock :: (Block, ByteString)
t_codeblock = ( CodeBlock ("id", ["kls"], [("k1", "v1"), ("k2", "v2")]) "Foo Bar"
              , [s|{"t":"CodeBlock","c":[["id",["kls"],[["k1","v1"],["k2","v2"]]],"Foo Bar"]}|]
              )

t_rawblock :: (Block, ByteString)
t_rawblock = ( RawBlock (Format "tex") "\\foo{bar}"
              , [s|{"t":"RawBlock","c":["tex","\\foo{bar}"]}|]
              )

t_blockquote :: (Block, ByteString)
t_blockquote = ( BlockQuote [Para [Str "Hello"]]
         , [s|{"t":"BlockQuote","c":[{"t":"Para","c":[{"t":"Str","c":"Hello"}]}]}|]
         )

t_orderedlist :: (Block, ByteString)
t_orderedlist = (OrderedList (1,Decimal,Period)
                 [[Para [Str "foo"]]
                 ,[Para [Str "bar"]]]
                , [s|{"t":"OrderedList","c":[[1,{"t":"Decimal"},{"t":"Period"}],[[{"t":"Para","c":[{"t":"Str","c":"foo"}]}],[{"t":"Para","c":[{"t":"Str","c":"bar"}]}]]]}|]
                    )

t_bulletlist :: (Block, ByteString)
t_bulletlist = (BulletList
                 [[Para [Str "foo"]]
                 ,[Para [Str "bar"]]]
               , [s|{"t":"BulletList","c":[[{"t":"Para","c":[{"t":"Str","c":"foo"}]}],[{"t":"Para","c":[{"t":"Str","c":"bar"}]}]]}|]
                   )

t_definitionlist :: (Block, ByteString)
t_definitionlist = (DefinitionList
                    [([Str "foo"],
                      [[Para [Str "bar"]]])
                    ,([Str "fizz"],
                      [[Para [Str "pop"]]])]
                   , [s|{"t":"DefinitionList","c":[[[{"t":"Str","c":"foo"}],[[{"t":"Para","c":[{"t":"Str","c":"bar"}]}]]],[[{"t":"Str","c":"fizz"}],[[{"t":"Para","c":[{"t":"Str","c":"pop"}]}]]]]}|]
                    )

t_header :: (Block, ByteString)
t_header = ( Header 2 ("id", ["kls"], [("k1", "v1"), ("k2", "v2")]) [Str "Head"]
           , [s|{"t":"Header","c":[2,["id",["kls"],[["k1","v1"],["k2","v2"]]],[{"t":"Str","c":"Head"}]]}|]
           )

t_row :: (Row, ByteString)
t_row = (Row ("id",["kls"],[("k1", "v1"), ("k2", "v2")])
         [Cell ("", [], []) AlignRight 2 3 [Para [Str "bar"]]]
        ,[s|[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[["",[],[]],{"t":"AlignRight"},2,3,[{"t":"Para","c":[{"t":"Str","c":"bar"}]}]]]]|])

t_caption :: (Caption, ByteString)
t_caption = (Caption ("id",["kls"],[("k1", "v1"), ("k2", "v2")])
             (Just [Str "foo"]) [Para [Str "bar"]]
            ,[s|[["id",["kls"],[["k1","v1"],["k2","v2"]]],[{"t":"Str","c":"foo"}],[{"t":"Para","c":[{"t":"Str","c":"bar"}]}]]|])

t_tablehead :: (TableHead, ByteString)
t_tablehead = (TableHead ("id",["kls"],[("k1", "v1"), ("k2", "v2")])
               [Row ("id",["kls"],[("k1", "v1"), ("k2", "v2")]) []]
              ,[s|[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[["id",["kls"],[["k1","v1"],["k2","v2"]]],[]]]]|])

t_tablebody :: (TableBody, ByteString)
t_tablebody = (TableBody ("id",["kls"],[("k1", "v1"), ("k2", "v2")]) 3
               [Row ("id",["kls"],[("k1", "v1"), ("k2", "v2")]) []]
               [Row ("id'",["kls'"],[("k1", "v1"), ("k2", "v2")]) []]
              ,[s|[["id",["kls"],[["k1","v1"],["k2","v2"]]],3,[[["id",["kls"],[["k1","v1"],["k2","v2"]]],[]]],[[["id'",["kls'"],[["k1","v1"],["k2","v2"]]],[]]]]|])

t_tablefoot :: (TableFoot, ByteString)
t_tablefoot = (TableFoot ("id",["kls"],[("k1", "v1"), ("k2", "v2")])
               [Row ("id",["kls"],[("k1", "v1"), ("k2", "v2")]) []]
              ,[s|[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[["id",["kls"],[["k1","v1"],["k2","v2"]]],[]]]]|])

t_cell :: (Cell, ByteString)
t_cell = (Cell ("id",["kls"],[("k1", "v1"), ("k2", "v2")]) AlignLeft 1 1
          [Para [Str "bar"]]
         ,[s|[["id",["kls"],[["k1","v1"],["k2","v2"]]],{"t":"AlignLeft"},1,1,[{"t":"Para","c":[{"t":"Str","c":"bar"}]}]]|])

t_rowheadcolumns :: (RowHeadColumns, ByteString)
t_rowheadcolumns = (1
                   ,[s|1|])

t_rowspan :: (RowSpan, ByteString)
t_rowspan = (1
            ,[s|1|])

t_colspan :: (ColSpan, ByteString)
t_colspan = (1
            ,[s|1|])

t_table :: (Block, ByteString)
t_table = ( Table
            ("id", ["kls"], [("k1", "v1"), ("k2", "v2")])
            [(AlignDefault,ColWidthDefault)
            ,(AlignRight,ColWidthDefault)
            ,(AlignLeft,ColWidthDefault)
            ,(AlignCenter,ColWidthDefault)
            ,(AlignDefault,ColWidthDefault)]
            (TableHead ("idh", ["klsh"], [("k1h", "v1h"), ("k2h", "v2h")])
             [tRow
              [tCell [Str "Head"]
              ,tCell [Str "Right"]
              ,tCell [Str "Left"]
              ,tCell [Str "Center"]
              ,tCell [Str "Default"]]])
            [TableBody ("idb", ["klsb"], [("k1b", "v1b"), ("k2b", "v2b")]) 1
             [tRow
              [tCell [Str "ihead12"]
              ,tCell [Str "i12"]
              ,tCell [Str "i12"]
              ,tCell [Str "i12"]
              ,tCell [Str "i12"]]]
             [tRow
              [tCell [Str "head12"]
              ,tCell' [Str "12"]
              ,tCell [Str "12"]
              ,tCell' [Str "12"]
              ,tCell [Str "12"]]
            ,tRow
              [tCell [Str "head123"]
              ,tCell [Str "123"]
              ,tCell [Str "123"]
              ,tCell [Str "123"]
              ,tCell [Str "123"]]
            ,tRow
              [tCell [Str "head1"]
              ,tCell [Str "1"]
              ,tCell [Str "1"]
              ,tCell [Str "1"]
              ,tCell [Str "1"]]]]
            (TableFoot ("idf", ["klsf"], [("k1f", "v1f"), ("k2f", "v2f")])
             [tRow
              [tCell [Str "foot"]
              ,tCell [Str "footright"]
              ,tCell [Str "footleft"]
              ,tCell [Str "footcenter"]
              ,tCell [Str "footdefault"]]])
          ,[s|{"t":"Table","c":[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[{"t":"AlignDefault"},{"t":"ColWidthDefault"}],[{"t":"AlignRight"},{"t":"ColWidthDefault"}],[{"t":"AlignLeft"},{"t":"ColWidthDefault"}],[{"t":"AlignCenter"},{"t":"ColWidthDefault"}],[{"t":"AlignDefault"},{"t":"ColWidthDefault"}]],[["idh",["klsh"],[["k1h","v1h"],["k2h","v2h"]]],[[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"Head"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"Right"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"Left"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"Center"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"Default"}]}]]]]]],[[["idb",["klsb"],[["k1b","v1b"],["k2b","v2b"]]],1,[[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"ihead12"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"i12"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"i12"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"i12"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"i12"}]}]]]]],[[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"head12"}]}]],[["id",["kls"],[["k1","v1"],["k2","v2"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"12"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"12"}]}]],[["id",["kls"],[["k1","v1"],["k2","v2"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"12"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"12"}]}]]]],[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"head123"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"123"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"123"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"123"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"123"}]}]]]],[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"head1"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"1"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"1"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"1"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"1"}]}]]]]]]],[["idf",["klsf"],[["k1f","v1f"],["k2f","v2f"]]],[[["id",["kls"],[["k1","v1"],["k2","v2"]]],[[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"foot"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"footright"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"footleft"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"footcenter"}]}]],[["a",["b"],[["c","d"],["e","f"]]],{"t":"AlignDefault"},1,1,[{"t":"Plain","c":[{"t":"Str","c":"footdefault"}]}]]]]]]]}|]
              )
  where
    tCell i = Cell ("a", ["b"], [("c", "d"), ("e", "f")]) AlignDefault 1 1 [Plain i]
    tCell' i = Cell ("id", ["kls"], [("k1", "v1"), ("k2", "v2")]) AlignDefault 1 1 [Plain i]
    tRow = Row ("id", ["kls"], [("k1", "v1"), ("k2", "v2")])

t_figure :: (Block, ByteString)
t_figure = (Figure
            ("id", ["kls"], [("k1", "v1"), ("k2", "v2")])
            CaptionBefore
            (Caption nullAttr (Just [Str "hello"]) [Para [Str "cap content"]])
            [Para [Str "fig content"]]
           ,[s|{"t":"Figure","c":[["id",["kls"],[["k1","v1"],["k2","v2"]]],{"t":"CaptionBefore"},[["",[],[]],[{"t":"Str","c":"hello"}],[{"t":"Para","c":[{"t":"Str","c":"cap content"}]}]],[{"t":"Para","c":[{"t":"Str","c":"fig content"}]}]]}|]
           )

t_div :: (Block, ByteString)
t_div = ( Div ("id", ["kls"], [("k1", "v1"), ("k2", "v2")]) [Para [Str "Hello"]]
         , [s|{"t":"Div","c":[["id",["kls"],[["k1","v1"],["k2","v2"]]],[{"t":"Para","c":[{"t":"Str","c":"Hello"}]}]]}|]
         )

t_null :: (Block, ByteString)
t_null = (Null, [s|{"t":"Null"}|])

-- headers and rows are padded to a consistent number of
-- cells in order to avoid syntax errors after conversion, see
-- jgm/pandoc#4059.
-- This may change as the table representation changes.
t_tableSan :: Test
t_tableSan = testCase "table sanitisation" assertion
             where assertion = assertEqual err expected generated
                   err = "sanitisation error"
                   generated = simpleTable
                                  [plain (text "foo"), plain (text "bar")]
                                  [[mempty]
                                  ,[]]
                   tCell i = Cell nullAttr AlignDefault 1 1 [Plain [Str i]]
                   emptyRow = Row nullAttr $ replicate 2 emptyCell
                   expected = singleton (Table
                                          nullAttr
                                          [(AlignDefault,ColWidthDefault)
                                          ,(AlignDefault,ColWidthDefault)]
                                          (TableHead nullAttr
                                           [Row nullAttr
                                            [tCell "foo"
                                            ,tCell "bar"]])
                                          [TableBody nullAttr 0
                                           []
                                           [emptyRow
                                           ,emptyRow]]
                                         (TableFoot nullAttr
                                          []))

withWidth :: Testable prop => (Int -> prop) -> Property
withWidth = forAll $ choose (2 :: Int, 16)

widthNormIsIdempotent :: (Arbitrary a, Show a, Eq a)
                      => (Int -> a -> a) -> Property
widthNormIsIdempotent f = withWidth $
  \n a -> let a' = f n a in f n a' == a'

p_tableNormHeadIdempotent :: Property
p_tableNormHeadIdempotent = widthNormIsIdempotent normalizeTableHead

p_tableNormBodyIdempotent :: Property
p_tableNormBodyIdempotent = widthNormIsIdempotent normalizeTableBody

p_tableNormFootIdempotent :: Property
p_tableNormFootIdempotent = widthNormIsIdempotent normalizeTableFoot

cellSubset :: Cell -> Cell -> Bool
cellSubset (Cell attr1 align1 rs1 cs1 body1) (Cell attr2 align2 rs2 cs2 body2)
  = and [ attr1 == attr2
        , align1 == align2
        , dimValid rs1 rs2
        , dimValid cs1 cs2
        , body1 == body2 ]
  where
    dimValid x y = (y < 1 && x == 1) || (x >= 1 && x <= y)

-- True when the first list is an initial segment of the second,
-- modulo cell subsetting and the appending of padding cells onto the
-- second.
cellsSubsetPad :: [Cell] -> [Cell] -> Bool
cellsSubsetPad (x:xs) (y:ys) = cellSubset x y && cellsSubsetPad xs ys
cellsSubsetPad xs _ = all isPadCell xs
  where
    isPadCell = (== emptyCell)

-- Only valid for the TableHead and TableFoot. See also
-- p_tableNormBodyIsSubset.
rowSubset :: Row -> Row -> Bool
rowSubset (Row a1 x1) (Row a2 x2) = a1 == a2 && cellsSubsetPad x1 x2

-- The remarks in rowSubset apply.
rowsSubset :: [Row] -> [Row] -> Bool
rowsSubset (x:xs) (y:ys) = rowSubset x y && rowsSubset xs ys
rowsSubset []     _      = True
rowsSubset (_:_)  []     = False

normIsSubset :: (Arbitrary a, Show a, Eq a)
             => (Int -> a -> a)
             -> (a -> [Row])
             -> Property
normIsSubset f proj = withWidth $
  \n a -> let a' = f n a in proj a' `rowsSubset` proj a

p_tableNormHeadIsSubset :: Property
p_tableNormHeadIsSubset = normIsSubset normalizeTableHead thproj
  where
    thproj (TableHead _ r) = r

-- Checking that each row is a subset of its unnormalized version is a
-- little onerous in the TableBody (because of the row head/row body
-- distinction), so we settle for testing it only for the first row of
-- the intermediate body. The intermediate head is still checked
-- fully.
p_tableNormBodyIsSubset :: Property
p_tableNormBodyIsSubset = withWidth $
  \n tb -> checkBody n (normalizeTableBody n tb) tb
  where
    cellLength (Cell _ _ _ (ColSpan w) _) = w
    cellLengths = sum . map cellLength
    gatherLen n = gatherLen' n 0
    gatherLen' n count (c:cs) | count < n
      = let (beg, end) = gatherLen' n (count + cellLength c) cs
        in (c : beg, end)
    gatherLen' _ _ cs = ([], cs)
    -- Gather as much of the head as we can from the new and old rows,
    -- then make sure the dimensions line up and the subsetting is
    -- correct.
    checkRow n rhc (Row _ r') (Row _ r)
      = let (rhead', rbody') = gatherLen rhc r'
            (rhead, rbody) = gatherLen rhc r
        in and [ cellLengths rhead' == rhc
               , rhc + cellLengths rbody' == n
               , cellsSubsetPad rhead' rhead
               , cellsSubsetPad rbody' rbody ]
    checkRows n rhc (r':_) (r:_) = checkRow n rhc r' r
    checkRows _ _   []     []    = True
    checkRows _ _   _      _     = False
    checkBody n (TableBody _ (RowHeadColumns rhc) th' tb') (TableBody _ _ th tb)
      = rowsSubset th' th && checkRows n rhc tb' tb

p_tableNormFootIsSubset :: Property
p_tableNormFootIsSubset = normIsSubset normalizeTableFoot tfproj
  where
    tfproj (TableFoot _ r) = r

-- True when the first row in a section (table head, table foot,
-- intermediate header, body of table body) has the correct
-- width. Only with the first row is it easy to check.
firstRowCorrectWidth :: Int -> [Row] -> [Row] -> Bool
firstRowCorrectWidth n (Row _ cs:_) (_:_) = n == sum (map cellLength cs)
  where cellLength (Cell _ _ _ (ColSpan w) _) = w
firstRowCorrectWidth _ []           []    = True
firstRowCorrectWidth _ _            _     = False

testRowCorrectWidth :: (Arbitrary a, Show a, Eq a)
                     => (Int -> a -> a)
                     -> (a -> [Row])
                     -> Property
testRowCorrectWidth f proj = withWidth $
  \n a -> let a' = f n a in firstRowCorrectWidth n (proj a') (proj a)

p_tableNormHeadRowWidth :: Property
p_tableNormHeadRowWidth = testRowCorrectWidth normalizeTableHead thproj
  where
    thproj (TableHead _ r) = r

p_tableNormBodyRowWidth :: Property
p_tableNormBodyRowWidth = withWidth $
  \n tb -> compBody n tb $ normalizeTableBody n tb
  where
    compBody n (TableBody _ _ th tb) (TableBody _ _ th' tb')
      = firstRowCorrectWidth n th' th && firstRowCorrectWidth n tb' tb

p_tableNormFootRowWidth :: Property
p_tableNormFootRowWidth = testRowCorrectWidth normalizeTableFoot tfproj
  where
    tfproj (TableFoot _ r) = r

t_tableNormExample :: Test
t_tableNormExample = testCase "table normalization example" assertion
  where
    assertion = assertEqual "normalization error" expected generated
    cl a h w = Cell (a, [], []) AlignDefault h w []
    rws = map $ Row nullAttr
    th = TableHead nullAttr . rws
    tb n x y = TableBody nullAttr n (rws x) (rws y)
    tf = TableFoot nullAttr . rws
    initialHeads =
      [[cl "a" 1 1,cl "b" 3 2]
      ,[cl "c" 2 2           ,cl "d" 1 1]
      ]
    finalHeads =
      [[cl "a" 1 1, cl "b" 2 2]
      ,[cl "c" 1 1]
      ]
    initialTB = tb 1
      [[]
      ,[cl "g" (-7) 0,cl "h" 4 1]]
      [[cl "e" 4 3   ,cl "f" 4 3]
      ,[]
      ,[emptyCell]
      ]
    finalTB = tb 1
      [[emptyCell,emptyCell,emptyCell]
      ,[cl "g" 1 1,cl "h" 1 1,emptyCell]]
      [[cl "e" 3 1,cl "f" 3 2]
      ,[]
      ,[]]
    spec = replicate 3 (AlignDefault, ColWidthDefault)
    expected = singleton $ Table nullAttr
                                 spec
                                 (th finalHeads)
                                 [finalTB]
                                 (tf finalHeads)
    generated = table spec (th initialHeads) [initialTB] (tf initialHeads)

p_figureRepresentation :: Property
p_figureRepresentation = forAll (arbitrary :: Gen [Inline]) (\figureCaption ->
  simpleFigureWith
      ("", [], [])
      (Builder.fromList figureCaption)
      "url"
      "title" ==
      Builder.fromList
          [Para [Image ("", [], []) figureCaption ("url", "fig:title") ]]
  )

tests :: [Test]
tests =
  [ testGroup "Walk"
    [ testProperty "p_walk inlineTrans" (p_walk inlineTrans)
    , testProperty "p_walk blockTrans" (p_walk blockTrans)
    , testProperty "p_query inlineQuery" (p_query inlineQuery)
    , testProperty "p_query blockQuery" (p_query blockQuery)
    , testProperty "p_walkList inlinesTrans"  (p_walkList inlinesTrans)
    , testProperty "p_queryList inlinesQuery" (p_queryList inlinesQuery)
    , testProperty "p_walkList blocksTrans"  (p_walkList blocksTrans)
    , testProperty "p_queryList blocksQuery" (p_queryList blocksQuery)
    ]
  , testGroup "JSON"
    [ testGroup "encoding/decoding properties"
      [ testProperty "round-trip" prop_roundtrip
      ]
    , testGroup "JSON encoding/decoding"
      [ testGroup "Meta"
        [ testEncodeDecode "Meta" t_meta
        , testEncodeDecode "MetaMap" t_metamap
        , testEncodeDecode "MetaList" t_metalist
        , testEncodeDecode "MetaBool" t_metabool
        , testEncodeDecode "MetaString" t_metastring
        , testEncodeDecode "MetaInlines" t_metainlines
        , testEncodeDecode "MetaBlocks" t_metablocks
        ]
      , testGroup "QuoteType"
        [ testEncodeDecode "SingleQuote" t_singlequote
        , testEncodeDecode "DoubleQuote" t_doublequote
        ]
      , testGroup "CitationType"
        [ testEncodeDecode "AuthorInText" t_authorintext
        , testEncodeDecode "SuppressAuthor" t_suppressauthor
        , testEncodeDecode "NormalCitation" t_normalcitation
        ]
      , testEncodeDecode "Citation" t_citation
      , testGroup "MathType"
        [ testEncodeDecode "DisplayMath" t_displaymath
        , testEncodeDecode "InlineMath" t_inlinemath
        ]
      , testGroup "Inline"
        [ testEncodeDecode "Str" t_str
        , testEncodeDecode "Emph" t_emph
        , testEncodeDecode "Underline" t_underline
        , testEncodeDecode "Strong" t_strong
        , testEncodeDecode "Strikeout" t_strikeout
        , testEncodeDecode "Superscript" t_superscript
        , testEncodeDecode "Subscript" t_subscript
        , testEncodeDecode "SmallCaps" t_smallcaps
        , testEncodeDecode "Quoted" t_quoted
        , testEncodeDecode "Cite" t_cite
        , testEncodeDecode "Code" t_code
        , testEncodeDecode "Space" t_space
        , testEncodeDecode "SoftBreak" t_softbreak
        , testEncodeDecode "LineBreak" t_linebreak
        , testEncodeDecode "RawInline" t_rawinline
        , testEncodeDecode "Link" t_link
        , testEncodeDecode "Image" t_image
        , testEncodeDecode "Note" t_note
        , testEncodeDecode "Span" t_span
        ]
      , testGroup "Block"
        [ testEncodeDecode "Plain" t_plain
        , testEncodeDecode "Para" t_para
        , testEncodeDecode "LineBlock" t_lineblock
        , testEncodeDecode "CodeBlock" t_codeblock
        , testEncodeDecode "RawBlock" t_rawblock
        , testEncodeDecode "BlockQuote" t_blockquote
        , testEncodeDecode "OrderedList" t_orderedlist
        , testEncodeDecode "BulletList" t_bulletlist
        , testEncodeDecode "DefinitionList" t_definitionlist
        , testEncodeDecode "Header" t_header
        , testEncodeDecode "Table" t_table
        , testEncodeDecode "Figure" t_figure
        , testEncodeDecode "Div" t_div
        , testEncodeDecode "Null" t_null
        ]
      , testGroup "Table"
        [ testEncodeDecode "Row" t_row
        , testEncodeDecode "Caption" t_caption
        , testEncodeDecode "TableHead" t_tablehead
        , testEncodeDecode "TableBody" t_tablebody
        , testEncodeDecode "TableFoot" t_tablefoot
        , testEncodeDecode "Cell" t_cell
        , testEncodeDecode "RowHeadColumns" t_rowheadcolumns
        , testEncodeDecode "RowSpan" t_rowspan
        , testEncodeDecode "ColSpan" t_colspan
        ]
      ]
    ]
  , testGroup "Table normalization"
    [ testProperty "p_tableNormHeadIdempotent" p_tableNormHeadIdempotent
    , testProperty "p_tableNormBodyIdempotent" p_tableNormBodyIdempotent
    , testProperty "p_tableNormFootIdempotent" p_tableNormFootIdempotent
    , testProperty "p_tableNormHeadIsSubset" p_tableNormHeadIsSubset
    , testProperty "p_tableNormBodyIsSubset" p_tableNormBodyIsSubset
    , testProperty "p_tableNormFootIsSubset" p_tableNormFootIsSubset
    , testProperty "p_tableNormHeadRowWidth" p_tableNormHeadRowWidth
    , testProperty "p_tableNormBodyRowWidth" p_tableNormBodyRowWidth
    , testProperty "p_tableNormFootRowWidth" p_tableNormFootRowWidth
    ]
  , t_tableSan
  , t_tableNormExample
  , testGroup "Figure"
    [ testProperty "p_figureRepresentation figure representation" p_figureRepresentation ]
  ]


main :: IO ()
main = defaultMain tests

