{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, CPP #-}

import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Builder (singleton, plain, text, simpleTable)
import Data.Generics
import Data.List (tails)
import Test.HUnit (Assertion, assertEqual, assertFailure)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
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
             , [s|{"citationSuffix":[{"t":"Space"},{"t":"Str","c":"123"}],"citationNoteNum":0,"citationMode":{"t":"NormalCitation"},"citationPrefix":[{"t":"Str","c":"cf"}],"citationId":"jameson:unconscious","citationHash":0}|]
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
         ,[s|{"t":"Cite","c":[[{"citationSuffix":[{"t":"Space"},{"t":"Str","c":"12"}],"citationNoteNum":0,"citationMode":{"t":"NormalCitation"},"citationPrefix":[{"t":"Str","c":"cf"}],"citationId":"jameson:unconscious","citationHash":0}],[{"t":"Str","c":"[cf"},{"t":"Space"},{"t":"Str","c":"@jameson:unconscious"},{"t":"Space"},{"t":"Str","c":"12]"}]]}|]
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

t_table :: (Block, ByteString)
t_table = (  Table
             [Str "Demonstration"
             ,Space
             ,Str "of"
             ,Space
             ,Str "simple"
             ,Space
             ,Str "table"
             ,Space
             ,Str "syntax."]
             [AlignRight
             ,AlignLeft
             ,AlignCenter
             ,AlignDefault]
             [0.0,0.0,0.0,0.0]
            [[Plain [Str "Right"]]
            ,[Plain [Str "Left"]]
            ,[Plain [Str "Center"]]
            ,[Plain [Str "Default"]]]
            [[[Plain [Str "12"]]
             ,[Plain [Str "12"]]
             ,[Plain [Str "12"]]
             ,[Plain [Str "12"]]]
            ,[[Plain [Str "123"]]
             ,[Plain [Str "123"]]
             ,[Plain [Str "123"]]
             ,[Plain [Str "123"]]]
            ,[[Plain [Str "1"]]
             ,[Plain [Str "1"]]
             ,[Plain [Str "1"]]
             ,[Plain [Str "1"]]]]
          ,
            [s|{"t":"Table","c":[[{"t":"Str","c":"Demonstration"},{"t":"Space"},{"t":"Str","c":"of"},{"t":"Space"},{"t":"Str","c":"simple"},{"t":"Space"},{"t":"Str","c":"table"},{"t":"Space"},{"t":"Str","c":"syntax."}],[{"t":"AlignRight"},{"t":"AlignLeft"},{"t":"AlignCenter"},{"t":"AlignDefault"}],[0,0,0,0],[[{"t":"Plain","c":[{"t":"Str","c":"Right"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Left"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Center"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Default"}]}]],[[[{"t":"Plain","c":[{"t":"Str","c":"12"}]}],[{"t":"Plain","c":[{"t":"Str","c":"12"}]}],[{"t":"Plain","c":[{"t":"Str","c":"12"}]}],[{"t":"Plain","c":[{"t":"Str","c":"12"}]}]],[[{"t":"Plain","c":[{"t":"Str","c":"123"}]}],[{"t":"Plain","c":[{"t":"Str","c":"123"}]}],[{"t":"Plain","c":[{"t":"Str","c":"123"}]}],[{"t":"Plain","c":[{"t":"Str","c":"123"}]}]],[[{"t":"Plain","c":[{"t":"Str","c":"1"}]}],[{"t":"Plain","c":[{"t":"Str","c":"1"}]}],[{"t":"Plain","c":[{"t":"Str","c":"1"}]}],[{"t":"Plain","c":[{"t":"Str","c":"1"}]}]]]]}|]
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
t_tableSan :: Test
t_tableSan = testCase "table sanitisation" assertion
             where assertion = assertEqual err expected generated
                   err = "sanitisation error"
                   generated = simpleTable
                                  [plain (text "foo"), plain (text "bar")]
                                  [[mempty]
                                  ,[]]
                   expected = singleton (Table
                                         []
                                         [AlignDefault, AlignDefault]
                                         [0.0, 0.0]
                                         [[Plain [Str "foo"]],
                                          [Plain [Str "bar"]]]
                                         [[[], []], [[], []]])


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
        [ testEncodeDecode "MetaMap" t_metamap
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
        , testEncodeDecode "Div" t_div
        , testEncodeDecode "Null" t_null
        ]
      ]
    ],
    t_tableSan
  ]


main :: IO ()
main = defaultMain tests
