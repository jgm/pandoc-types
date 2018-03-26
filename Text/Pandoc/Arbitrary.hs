{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
-- provides Arbitrary instance for Pandoc types
module Text.Pandoc.Arbitrary ()
where
import Test.QuickCheck
import Control.Monad (forM)
import Text.Pandoc.Definition
import Text.Pandoc.Builder

realString :: Gen String
realString = resize 8 $ listOf $ frequency [ (9, elements [' '..'\127'])
                                           , (1, elements ['\128'..'\9999']) ]

arbAttr :: Gen Attr
arbAttr = do
  id' <- elements ["","loc"]
  classes <- elements [[],["haskell"],["c","numberLines"]]
  keyvals <- elements [[],[("start","22")],[("a","11"),("b_2","a b c")]]
  return (id',classes,keyvals)

instance Arbitrary Inlines where
  arbitrary = (fromList :: [Inline] -> Inlines) <$> arbitrary

instance Arbitrary Blocks where
  arbitrary = (fromList :: [Block] -> Blocks) <$> arbitrary

instance Arbitrary Inline where
  arbitrary = resize 3 $ arbInline 2

arbInlines :: Int -> Gen [Inline]
arbInlines n = listOf1 (arbInline n) `suchThat` (not . startsWithSpace)
  where startsWithSpace (Space:_) = True
        startsWithSpace        _  = False

-- restrict to 3 levels of nesting max; otherwise we get
-- bogged down in indefinitely large structures
arbInline :: Int -> Gen Inline
arbInline n = frequency $ [ (60, Str <$> realString)
                          , (60, pure Space)
                          , (10, Code <$> arbAttr <*> realString)
                          , (5,  elements [ RawInline (Format "html") "<a id=\"eek\">"
                                          , RawInline (Format "latex") "\\my{command}" ])
                          ] ++ [ x | x <- nesters, n > 1]
   where nesters = [ (10, Emph <$> arbInlines (n-1))
                   , (10, Strong <$> arbInlines (n-1))
                   , (10, Strikeout <$> arbInlines (n-1))
                   , (10, Superscript <$> arbInlines (n-1))
                   , (10, Subscript <$> arbInlines (n-1))
                   , (10, SmallCaps <$> arbInlines (n-1))
                   , (10, Span <$> arbAttr <*> arbInlines (n-1))
                   , (10, Quoted <$> arbitrary <*> arbInlines (n-1))
                   , (10, Math <$> arbitrary <*> realString)
                   , (10, Link <$> arbAttr <*> arbInlines (n-1) <*> ((,) <$> realString <*> realString))
                   , (10, Image <$> arbAttr <*> arbInlines (n-1) <*> ((,) <$> realString <*> realString))
                   , (2,  Cite <$> arbitrary <*> arbInlines 1)
                   , (2,  Note <$> resize 3 (listOf1 $ arbBlock (n-1)))
                   ]

instance Arbitrary Block where
  arbitrary = resize 3 $ arbBlock 2

arbBlock :: Int -> Gen Block
arbBlock n = frequency $ [ (10, Plain <$> arbInlines (n-1))
                         , (15, Para <$> arbInlines (n-1))
                         , (5,  CodeBlock <$> arbAttr <*> realString)
                         , (3,  LineBlock <$>
                                ((:) <$>
                                  arbInlines ((n - 1) `mod` 3) <*>
                                  forM [1..((n - 1) `div` 3)] (const (arbInlines 3))))
                         , (2,  elements [ RawBlock (Format "html")
                                            "<div>\n*&amp;*\n</div>"
                                         , RawBlock (Format "latex")
                                            "\\begin[opt]{env}\nhi\n{\\end{env}"
                                         ])
                         , (5,  Header <$> choose (1 :: Int, 6)
                                       <*> pure nullAttr
                                       <*> arbInlines (n-1))
                         , (2,  pure HorizontalRule)
                         ] ++ [x | x <- nesters, n > 0]
   where nesters = [ (5, BlockQuote <$> listOf1 (arbBlock (n-1)))
                   , (5, OrderedList <$> ((,,) <$> (arbitrary `suchThat` (> 0))
                                                <*> arbitrary
                                                <*> arbitrary)
                                      <*> listOf1 (listOf1 $ arbBlock (n-1)))
                   , (5, BulletList <$> listOf1 (listOf1 $ arbBlock (n-1)))
                   , (5, DefinitionList <$> listOf1 ((,) <$> arbInlines (n-1)
                                                          <*> listOf1 (listOf1 $ arbBlock (n-1))))
                   , (5, Div <$> arbAttr <*> listOf1 (arbBlock (n-1)))
                   , (2, do rs <- choose (1 :: Int, 4)
                            cs <- choose (1 :: Int, 4)
                            Table <$> arbInlines (n-1)
                                  <*> vector cs
                                  <*> vectorOf cs (elements [0, 0.25])
                                  <*> vectorOf cs (listOf $ arbBlock (n-1))
                                  <*> vectorOf rs (vectorOf cs $ listOf $ arbBlock (n-1)))
                   ]

instance Arbitrary Pandoc where
        arbitrary = resize 8 (Pandoc <$> arbitrary <*> arbitrary)

instance Arbitrary CitationMode where
        arbitrary
          = do x <- choose (0 :: Int, 2)
               case x of
                   0 -> return AuthorInText
                   1 -> return SuppressAuthor
                   2 -> return NormalCitation
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary Citation where
        arbitrary
          = Citation <$> listOf (elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_'])
                     <*> arbInlines 1
                     <*> arbInlines 1
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary MathType where
        arbitrary
          = do x <- choose (0 :: Int, 1)
               case x of
                   0 -> return DisplayMath
                   1 -> return InlineMath
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary QuoteType where
        arbitrary
          = do x <- choose (0 :: Int, 1)
               case x of
                   0 -> return SingleQuote
                   1 -> return DoubleQuote
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary Meta where
        arbitrary
          = do (x1 :: Inlines) <- arbitrary
               (x2 :: [Inlines]) <- filter (not . isNull) <$> arbitrary
               (x3 :: Inlines) <- arbitrary
               return $ setMeta "title" x1
                      $ setMeta "author" x2
                      $ setMeta "date" x3
                      $ nullMeta

instance Arbitrary Alignment where
        arbitrary
          = do x <- choose (0 :: Int, 3)
               case x of
                   0 -> return AlignLeft
                   1 -> return AlignRight
                   2 -> return AlignCenter
                   3 -> return AlignDefault
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary ListNumberStyle where
        arbitrary
          = do x <- choose (0 :: Int, 6)
               case x of
                   0 -> return DefaultStyle
                   1 -> return Example
                   2 -> return Decimal
                   3 -> return LowerRoman
                   4 -> return UpperRoman
                   5 -> return LowerAlpha
                   6 -> return UpperAlpha
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary ListNumberDelim where
        arbitrary
          = do x <- choose (0 :: Int, 3)
               case x of
                   0 -> return DefaultDelim
                   1 -> return Period
                   2 -> return OneParen
                   3 -> return TwoParens
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"
