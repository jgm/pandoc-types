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

shrinkText :: Text -> [Text]
shrinkText xs = T.pack <$> shrink (T.unpack xs)

shrinkText2 :: (Text, Text) -> [(Text, Text)]
shrinkText2 = liftShrink2 shrinkText shrinkText

arbAttr :: Gen Attr
arbAttr = do
  id' <- elements ["","loc"]
  classes' <- elements [[],["haskell"],["c","numberLines"]]
  keyvals <- elements [[],[("start","22")],[("a","11"),("b_2","a b c")]]
  return (id',classes',keyvals)

shrinkAttr :: Attr -> [Attr]
shrinkAttr (a, b, c)
  = [ (a', b', c') | a' <- shrinkText a,
                     b' <- liftShrink shrinkText b,
                     c' <- liftShrink shrinkText2 c ]

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
          flattenInline (Underline ils) = ils
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
          flattenBlock (Table _ _ hd bd ft) = flattenTableHead hd <>
                                              concatMap flattenTableBody bd <>
                                              flattenTableFoot ft
          flattenBlock (Figure _ _ capt blks) = flattenCaption capt <> blks
          flattenBlock (Div _ blks) = blks
          flattenBlock Null = []

          flattenCaption (Caption _ Nothing body)    = body
          flattenCaption (Caption _ (Just ils) body) = Para ils : body

          flattenTableHead (TableHead _ body) = flattenRows body
          flattenTableBody (TableBody _ _ hd bd) = flattenRows hd <> flattenRows bd
          flattenTableFoot (TableFoot _ body) = flattenRows body

          flattenRows = concatMap flattenRow
          flattenRow (Row _ body) = concatMap flattenCell body
          flattenCell (Cell _ _ _ _ blks) = blks

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
  shrink (Str s) = Str <$> shrinkText s
  shrink (Emph ils) = Emph <$> shrinkInlineList ils
  shrink (Underline ils) = Underline <$> shrinkInlineList ils
  shrink (Strong ils) = Strong <$> shrinkInlineList ils
  shrink (Strikeout ils) = Strikeout <$> shrinkInlineList ils
  shrink (Superscript ils) = Superscript <$> shrinkInlineList ils
  shrink (Subscript ils) = Subscript <$> shrinkInlineList ils
  shrink (SmallCaps ils) = SmallCaps <$> shrinkInlineList ils
  shrink (Quoted qtype ils) = Quoted qtype <$> shrinkInlineList ils
  shrink (Cite cits ils) = (Cite cits <$> shrinkInlineList ils)
                        ++ (flip Cite ils <$> shrink cits)
  shrink (Code attr s) = (Code attr <$> shrinkText s)
                      ++ (flip Code s <$> shrinkAttr attr)
  shrink Space = []
  shrink SoftBreak = []
  shrink LineBreak = []
  shrink (Math mtype s) = Math mtype <$> shrinkText s
  shrink (RawInline fmt s) = RawInline fmt <$> shrinkText s
  shrink (Link attr ils target) = [Link attr ils' target | ils' <- shrinkInlineList ils]
                               ++ [Link attr ils target' | target' <- shrinkText2 target]
                               ++ [Link attr' ils target | attr' <- shrinkAttr attr]
  shrink (Image attr ils target) = [Image attr ils' target | ils' <- shrinkInlineList ils]
                                ++ [Image attr ils target' | target' <- shrinkText2 target]
                                ++ [Image attr' ils target | attr' <- shrinkAttr attr]
  shrink (Note blks) = Note <$> shrinkBlockList blks
  shrink (Span attr s) = (Span attr <$> shrink s)
                      ++ (flip Span s <$> shrinkAttr attr)

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
                   , (10, Underline <$> arbInlines (n-1))
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
  shrink (CodeBlock attr s) = (CodeBlock attr <$> shrinkText s)
                           ++ (flip CodeBlock s <$> shrinkAttr attr)
  shrink (RawBlock fmt s) = RawBlock fmt <$> shrinkText s
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
                            ++ (flip (Header n) ils <$> shrinkAttr attr)
  shrink HorizontalRule = []
  shrink (Table attr specs thead tbody tfoot) =
    -- TODO: shrink number of columns
    [Table attr' specs thead tbody tfoot | attr' <- shrinkAttr attr] ++
    [Table attr specs thead' tbody tfoot | thead' <- shrink thead] ++
    [Table attr specs thead tbody' tfoot | tbody' <- shrink tbody] ++
    [Table attr specs thead tbody tfoot' | tfoot' <- shrink tfoot]
  shrink (Figure attr cp capt blks) =
    [Figure attr cp capt blks' | blks' <- shrinkBlockList blks] ++
    [Figure attr cp capt' blks | capt' <- shrink capt] ++
    [Figure attr' cp capt blks | attr' <- shrinkAttr attr]
  shrink (Div attr blks) = (Div attr <$> shrinkBlockList blks)
                        ++ (flip Div blks <$> shrinkAttr attr)
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
                   , (2, do cs <- choose (1 :: Int, 6)
                            bs <- choose (0 :: Int, 2)
                            Table <$> arbAttr
                                  <*> vectorOf cs ((,) <$> arbitrary
                                                       <*> elements [ ColWidthDefault
                                                                    , ColWidth (1/3)
                                                                    , ColWidth 0.25 ])
                                  <*> arbTableHead (n-1)
                                  <*> vectorOf bs (arbTableBody (n-1))
                                  <*> arbTableFoot (n-1))
                   , (2, Figure <$> arbAttr
                                <*> arbitrary
                                <*> arbitrary
                                <*> listOf1 (arbBlock (n-1)))
                   ]

arbRow :: Int -> Gen Row
arbRow n = do
  cs <- choose (0, 5)
  Row <$> arbAttr <*> vectorOf cs (arbCell n)

arbTableHead :: Int -> Gen TableHead
arbTableHead n = do
  rs <- choose (0, 5)
  TableHead <$> arbAttr <*> vectorOf rs (arbRow n)

arbTableBody :: Int -> Gen TableBody
arbTableBody n = do
  hrs <- choose (0 :: Int, 2)
  rs <- choose (0, 5)
  rhc <- choose (0, 5)
  TableBody <$> arbAttr
            <*> pure (RowHeadColumns rhc)
            <*> vectorOf hrs (arbRow n)
            <*> vectorOf rs (arbRow n)

arbTableFoot :: Int -> Gen TableFoot
arbTableFoot n = do
    rs <- choose (0, 5)
    TableFoot <$> arbAttr <*> vectorOf rs (arbRow n)

arbCell :: Int -> Gen Cell
arbCell n = Cell <$> arbAttr
                 <*> arbitrary
                 <*> (RowSpan <$> choose (1 :: Int, 2))
                 <*> (ColSpan <$> choose (1 :: Int, 2))
                 <*> listOf (arbBlock n)

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

instance Arbitrary Row where
  arbitrary = resize 3 $ arbRow 2
  shrink (Row attr body)
    = [Row attr' body | attr' <- shrinkAttr attr] ++
      [Row attr body' | body' <- shrink body]

instance Arbitrary TableHead where
  arbitrary = resize 3 $ arbTableHead 2
  shrink (TableHead attr body)
    = [TableHead attr' body | attr' <- shrinkAttr attr] ++
      [TableHead attr body' | body' <- shrink body]

instance Arbitrary TableBody where
  arbitrary = resize 3 $ arbTableBody 2
  -- TODO: shrink rhc?
  shrink (TableBody attr rhc hd bd)
    = [TableBody attr' rhc hd bd | attr' <- shrinkAttr attr] ++
      [TableBody attr rhc hd' bd | hd' <- shrink hd] ++
      [TableBody attr rhc hd bd' | bd' <- shrink bd]

instance Arbitrary TableFoot where
  arbitrary = resize 3 $ arbTableFoot 2
  shrink (TableFoot attr body)
    = [TableFoot attr' body | attr' <- shrinkAttr attr] ++
      [TableFoot attr body' | body' <- shrink body]

instance Arbitrary Cell where
  arbitrary = resize 3 $ arbCell 2
  shrink (Cell attr malign h w body)
    = [Cell attr malign h w body' | body' <- shrinkBlockList body] ++
      [Cell attr' malign h w body | attr' <- shrinkAttr attr] ++
      [Cell attr malign' h w body | malign' <- shrink malign]

instance Arbitrary Caption where
  arbitrary = Caption <$> arbAttr <*> arbitrary <*> arbitrary
  shrink (Caption attr mshort body)
    = [Caption attr mshort' body | mshort' <- shrink mshort] ++
      [Caption attr mshort body' | body' <- shrinkBlockList body] ++
      [Caption attr' mshort body | attr' <- shrinkAttr attr]

instance Arbitrary CaptionPos where
        arbitrary
          = arbitraryBoundedEnum

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
               (x2 :: [Inlines]) <- filter (not . null) <$> arbitrary
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
