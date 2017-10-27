{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables,
             UndecidableInstances, OverloadedStrings #-}
-- provides Arbitrary instance for Pandoc types
module Text.Pandoc.Arbitrary ()
where
import Data.String
import Test.QuickCheck
import Control.Monad (forM, liftM, liftM2)
import Text.Pandoc.Definition
import Text.Pandoc.Builder

realString :: IsString string => Gen string
realString = fmap fromString $ resize 8 $ listOf
                               $ frequency [ (9, elements [' '..'\127'])
                                           , (1, elements ['\128'..'\9999']) ]

arbAttr :: IsString string => Gen (Attr' string)
arbAttr = do
  id' <- elements ["","loc"]
  classes <- elements [[],["haskell"],["c","numberLines"]]
  keyvals <- elements [[],[("start","22")],[("a","11"),("b_2","a b c")]]
  return $ Attr (id',classes,keyvals)

instance IsString string => Arbitrary (Inlines' string) where
  arbitrary = liftM (fromList :: [Inline' string] -> Inlines' string) arbitrary

instance IsString string => Arbitrary (Blocks' string) where
  arbitrary = liftM (fromList :: [Block' string] -> Blocks' string) arbitrary

instance IsString string => Arbitrary (Inline' string) where
  arbitrary = resize 3 $ arbInline 2

arbInlines :: IsString string => Int -> Gen [Inline' string]
arbInlines n = listOf1 (arbInline n) `suchThat` (not . startsWithSpace)
  where startsWithSpace (Space:_) = True
        startsWithSpace        _  = False

-- restrict to 3 levels of nesting max; otherwise we get
-- bogged down in indefinitely large structures
arbInline :: forall string. IsString string => Int -> Gen (Inline' string)
arbInline n = frequency $ [ (60, liftM Str realString)
                          , (60, return Space)
                          , (10, liftM2 Code arbAttr realString)
                          , (5,  elements [ RawInline (Format "html") "<a id=\"eek\">"
                                          , RawInline (Format "latex") "\\my{command}" ])
                          ] ++ [ x | x <- nesters, n > 1]
   where nesters :: [(Int, Gen (Inline' string))]
         nesters = [ (10,  liftM Emph $ arbInlines (n-1))
                   , (10,  liftM Strong $ arbInlines (n-1))
                   , (10,  liftM Strikeout $ arbInlines (n-1))
                   , (10,  liftM Superscript $ arbInlines (n-1))
                   , (10,  liftM Subscript $ arbInlines (n-1))
                   , (10,  liftM SmallCaps $ arbInlines (n-1))
                   , (10,  do x1 <- arbAttr
                              x2 <- arbInlines (n-1)
                              return $ Span x1 x2)
                   , (10,  do x1 <- arbitrary
                              x2 <- arbInlines (n-1)
                              return $ Quoted x1 x2)
                   , (10,  do x1 <- arbitrary
                              x2 <- realString
                              return $ Math x1 x2)
                   , (10,  do x0 <- arbAttr
                              x1 <- arbInlines (n-1)
                              x3 <- realString
                              x2 <- realString
                              return $ Link x0 x1 (x2,x3))
                   , (10,  do x0 <- arbAttr
                              x1 <- arbInlines (n-1)
                              x3 <- realString
                              x2 <- realString
                              return $ Image x0 x1 (x2,x3))
                   , (2,  liftM2 Cite arbitrary (arbInlines 1))
                   , (2,  liftM Note $ resize 3 $ listOf1 $ arbBlock (n-1))
                   ]

instance IsString string => Arbitrary (Block' string) where
  arbitrary = resize 3 $ arbBlock 2

arbBlock :: IsString string => Int -> Gen (Block' string)
arbBlock n = frequency $ [ (10, liftM Plain $ arbInlines (n-1))
                         , (15, liftM Para $ arbInlines (n-1))
                         , (5,  liftM2 CodeBlock arbAttr realString)
                         , (3,  liftM LineBlock $
                                  liftM2 (:)
                                         (arbInlines $ (n - 1) `mod` 3)
                                         (forM [1..((n - 1) `div` 3)]
                                               (const $ arbInlines 3)))
                         , (2,  elements [ RawBlock (Format "html")
                                            "<div>\n*&amp;*\n</div>"
                                         , RawBlock (Format "latex")
                                            "\\begin[opt]{env}\nhi\n{\\end{env}"
                                         ])
                         , (5,  do x1 <- choose (1 :: Int, 6)
                                   x2 <- arbInlines (n-1)
                                   return (Header x1 nullAttr x2))
                         , (2, return HorizontalRule)
                         ] ++ [x | x <- nesters, n > 0]
   where nesters = [ (5,  liftM BlockQuote $ listOf1 $ arbBlock (n-1))
                   , (5,  do x2 <- arbitrary
                             x3 <- arbitrary
                             x1 <- arbitrary `suchThat` (> 0)
                             x4 <- listOf1 $ listOf1 $ arbBlock (n-1)
                             return $ OrderedList (x1,x2,x3) x4 )
                   , (5,  liftM BulletList $ (listOf1 $ listOf1 $ arbBlock (n-1)))
                   , (5,  do items <- listOf1 $ do
                                        x1 <- listOf1 $ listOf1 $ arbBlock (n-1)
                                        x2 <- arbInlines (n-1)
                                        return (x2,x1)
                             return $ DefinitionList items)
                   , (5,  do x1 <- arbAttr
                             x2 <- listOf1 $ arbBlock (n-1)
                             return $ Div x1 x2)
                   , (2, do rs <- choose (1 :: Int, 4)
                            cs <- choose (1 :: Int, 4)
                            x1 <- arbInlines (n-1)
                            x2 <- vector cs
                            x3 <- vectorOf cs $ elements [0, 0.25]
                            x4 <- vectorOf cs $ listOf $ arbBlock (n-1)
                            x5 <- vectorOf rs $ vectorOf cs
                                  $ listOf $ arbBlock (n-1)
                            return (Table x1 x2 x3 x4 x5))
                   ]

instance (Ord string, IsString string) => Arbitrary (Pandoc' string) where
        arbitrary = resize 8 $ liftM2 Pandoc arbitrary arbitrary

instance Arbitrary CitationMode where
        arbitrary
          = do x <- choose (0 :: Int, 2)
               case x of
                   0 -> return AuthorInText
                   1 -> return SuppressAuthor
                   2 -> return NormalCitation
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance IsString string => Arbitrary (Citation' string) where
        arbitrary
          = do x1 <- fmap fromString $ listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
               x2 <- arbInlines 1
               x3 <- arbInlines 1
               x4 <- arbitrary
               x5 <- arbitrary
               x6 <- arbitrary
               return (Citation x1 x2 x3 x4 x5 x6)

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

instance (Ord string, IsString string) => Arbitrary (Meta' string) where
        arbitrary
          = do (x1 :: Inlines' string) <- arbitrary
               (x2 :: [Inlines' string]) <- liftM (filter (not . isNull)) arbitrary
               (x3 :: Inlines' string) <- arbitrary
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
