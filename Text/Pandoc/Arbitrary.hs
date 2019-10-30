{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}
-- provides Arbitrary instance for Pandoc types
module Text.Pandoc.Arbitrary ()
where
import Test.QuickCheck
import Control.Applicative (Applicative ((<*>), pure), (<$>))
import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Builder

realString :: Gen Text
realString = fmap T.pack $ resize 8 $ listOf $ frequency [ (9, elements [' '..'\127'])
                                                         , (1, elements ['\128'..'\9999']) ]

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

arbAttr :: Gen Attr
arbAttr = do
  id' <- elements ["","loc"]
  classes' <- elements [[],["haskell"],["c","numberLines"]]
  keyvals <- elements [[],[("start","22")],[("a","11"),("b_2","a b c")]]
  return (id',classes',keyvals)

instance Arbitrary Inlines where
  arbitrary = (fromList :: [Inline] -> Inlines) <$> arbitrary
  shrink = fmap fromList . ((++) <$> shrink <*> flattenShrinkInlines) . toList
    where flattenShrinkInlines (x:xs) =
            let x' = flattenInline x
            in (if null x' then [] else [x' ++ xs]) ++ [x:xs' | xs' <- flattenShrinkInlines xs]
          flattenShrinkInlines [] = []
          flattenInline :: Inline -> [Inline]
          flattenInline (Str _) = []
          flattenInline (Emph ils) = ils
          flattenInline (Strong ils) = ils
          flattenInline (Strikeout ils) = ils
          flattenInline (Superscript ils) = ils
          flattenInline (Subscript ils) = ils
          flattenInline (SmallCaps ils) = ils
          flattenInline (Quoted _ ils) = ils
          flattenInline (Cite _ ils) = ils
          flattenInline Code{} = []
          flattenInline Space = []
          flattenInline SoftBreak = []
          flattenInline LineBreak = []
          flattenInline Math{} = []
          flattenInline RawInline{} = []
          flattenInline (Link _ ils _) = ils
          flattenInline (Image _ ils _) = ils
          flattenInline Note{} = []
          flattenInline (Span _ ils) = ils

instance Arbitrary Blocks where
  arbitrary = (fromList :: [Block] -> Blocks) <$> arbitrary
  shrink = fmap fromList . ((++) <$> shrink <*> flattenShrinkBlocks) . toList
    where flattenShrinkBlocks (x:xs) =
            let x' = flattenBlock x
            in (if null x' then [] else [x' ++ xs]) ++ [x:xs' | xs' <- flattenShrinkBlocks xs]
          flattenShrinkBlocks [] = []
          flattenBlock :: Block -> [Block]
          flattenBlock Plain{} = []
          flattenBlock Para{} = []
          flattenBlock (LineBlock lns) = [Para x | x <- lns]
          flattenBlock CodeBlock{} = []
          flattenBlock RawBlock{} = []
          flattenBlock (BlockQuote blks) = blks
          flattenBlock (OrderedList _ blksList) = concat blksList
          flattenBlock (BulletList blksList) = concat blksList
          flattenBlock (DefinitionList defs) = concat [Para ils:concat blks | (ils, blks) <- defs]
          flattenBlock (Header _ _ ils) = [Para ils]
          flattenBlock HorizontalRule = []
          flattenBlock (Table caption _ _ cells rows) = Para caption : concat (concat $ cells:rows)
          flattenBlock (Div _ blks) = blks
          flattenBlock Null = []

shrinkInlineList :: [Inline] -> [[Inline]]
shrinkInlineList = fmap toList . shrink . fromList

shrinkInlinesList :: [[Inline]] -> [[[Inline]]]
shrinkInlinesList = fmap (fmap toList) . shrink . fmap fromList

shrinkBlockList :: [Block] -> [[Block]]
shrinkBlockList = fmap toList . shrink . fromList

shrinkBlocksList :: [[Block]] -> [[[Block]]]
shrinkBlocksList = fmap (fmap toList) . shrink . fmap fromList

instance Arbitrary Inline where
  arbitrary = resize 3 $ arbInline 2
  shrink (Str s) = Str <$> shrink s
  shrink (Emph ils) = Emph <$> shrinkInlineList ils
  shrink (Strong ils) = Strong <$> shrinkInlineList ils
  shrink (Strikeout ils) = Strikeout <$> shrinkInlineList ils
  shrink (Superscript ils) = Superscript <$> shrinkInlineList ils
  shrink (Subscript ils) = Subscript <$> shrinkInlineList ils
  shrink (SmallCaps ils) = SmallCaps <$> shrinkInlineList ils
  shrink (Quoted qtype ils) = Quoted qtype <$> shrinkInlineList ils
  shrink (Cite cits ils) = (Cite cits <$> shrinkInlineList ils)
                        ++ (flip Cite ils <$> shrink cits)
  shrink (Code attr s) = (Code attr <$> shrink s)
                      ++ (flip Code s <$> shrink attr)
  shrink Space = []
  shrink SoftBreak = []
  shrink LineBreak = []
  shrink (Math mtype s) = Math mtype <$> shrink s
  shrink (RawInline fmt s) = RawInline fmt <$> shrink s
  shrink (Link attr ils target) = [Link attr ils' target | ils' <- shrinkInlineList ils]
                               ++ [Link attr ils target' | target' <- shrink target]
                               ++ [Link attr' ils target | attr' <- shrink attr]
  shrink (Image attr ils target) = [Image attr ils' target | ils' <- shrinkInlineList ils]
                                ++ [Image attr ils target' | target' <- shrink target]
                                ++ [Image attr' ils target | attr' <- shrink attr]
  shrink (Note blks) = Note <$> shrinkBlockList blks
  shrink (Span attr s) = (Span attr <$> shrink s)
                      ++ (flip Span s <$> shrink attr)

arbInlines :: Int -> Gen [Inline]
arbInlines n = listOf1 (arbInline n) `suchThat` (not . startsWithSpace)
  where startsWithSpace (Space:_)     = True
        startsWithSpace (SoftBreak:_) = True
        -- Note: no LineBreak, similarly to Text.Pandoc.Builder (trimInlines)
        startsWithSpace _             = False

-- restrict to 3 levels of nesting max; otherwise we get
-- bogged down in indefinitely large structures
arbInline :: Int -> Gen Inline
arbInline n = frequency $ [ (60, Str <$> realString)
                          , (40, pure Space)
                          , (10, pure SoftBreak)
                          , (10, pure LineBreak)
                          , (10, Code <$> arbAttr <*> realString)
                          , (5,  elements [ RawInline (Format "html") "<a id=\"eek\">"
                                          , RawInline (Format "latex") "\\my{command}" ])
                          ] ++ [ x | n > 1, x <- nesters]
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
  shrink (Plain ils) = Plain <$> shrinkInlineList ils
  shrink (Para ils) = Para <$> shrinkInlineList ils
  shrink (LineBlock lns) = LineBlock <$> shrinkInlinesList lns
  shrink (CodeBlock attr s) = (CodeBlock attr <$> shrink s)
                           ++ (flip CodeBlock s <$> shrink attr)
  shrink (RawBlock fmt s) = RawBlock fmt <$> shrink s
  shrink (BlockQuote blks) = BlockQuote <$> shrinkBlockList blks
  shrink (OrderedList listAttrs blksList) = OrderedList listAttrs <$> shrinkBlocksList blksList
  shrink (BulletList blksList) = BulletList <$> shrinkBlocksList blksList
  shrink (DefinitionList defs) = DefinitionList <$> shrinkDefinitionList defs
    where shrinkDefinition (ils, blksList) = [(ils', blksList) | ils' <- shrinkInlineList ils]
                                          ++ [(ils, blksList') | blksList' <- shrinkBlocksList blksList]
          shrinkDefinitionList (x:xs) = [xs]
                                     ++ [x':xs | x' <- shrinkDefinition x]
                                     ++ [x:xs' | xs' <- shrinkDefinitionList xs]
          shrinkDefinitionList [] = []
  shrink (Header n attr ils) = (Header n attr <$> shrinkInlineList ils)
                            ++ (flip (Header n) ils <$> shrink attr)
  shrink HorizontalRule = []
  shrink (Table caption aligns widths cells rows) =
    -- TODO: shrink number of columns
    -- Shrink header contents
    [Table caption aligns widths cells' rows | cells' <- shrinkRow cells] ++
    -- Shrink number of rows and row contents
    [Table caption aligns widths cells rows' | rows' <- shrinkRows rows] ++
    -- Shrink caption
    [Table caption' aligns widths cells rows | caption' <- shrinkInlineList caption]
    where -- Shrink row contents without reducing the number of columns
          shrinkRow :: [TableCell] -> [[TableCell]]
          shrinkRow (x:xs) = [x':xs | x' <- shrinkBlockList x]
                          ++ [x:xs' | xs' <- shrinkRow xs]
          shrinkRow [] = []
          shrinkRows :: [[TableCell]] -> [[[TableCell]]]
          shrinkRows (x:xs) = [xs] -- Shrink number of rows
                           ++ [x':xs | x' <- shrinkRow x] -- Shrink row contents
                           ++ [x:xs' | xs' <- shrinkRows xs]
          shrinkRows [] = []
  shrink (Div attr blks) = (Div attr <$> shrinkBlockList blks)
                        ++ (flip Div blks <$> shrink attr)
  shrink Null = []

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
                         ] ++ [x | n > 0, x <- nesters]
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
          = Citation <$> fmap T.pack (listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_'])
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
                        nullMeta

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
