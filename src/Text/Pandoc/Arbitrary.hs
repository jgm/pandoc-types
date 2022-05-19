{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}
-- provides Arbitrary instance for Pandoc types
module Text.Pandoc.Arbitrary ()
where
import Test.QuickCheck hiding (shrinkList)
import qualified Test.QuickCheck as QC
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
            let x' = flattenInline $ unInline x
            in (if null x' then [] else [x' ++ xs]) ++ [x:xs' | xs' <- flattenShrinkInlines xs]
          flattenShrinkInlines [] = []
          flattenInline :: InlineF Block Inline -> [Inline]
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
            let x' = flattenBlock $ unBlock x
            in (if null x' then [] else [x' ++ xs]) ++ [x:xs' | xs' <- flattenShrinkBlocks xs]
          flattenShrinkBlocks [] = []
          flattenBlock :: BlockF Inline Block -> [Block]
          flattenBlock Plain{} = []
          flattenBlock Para{} = []
          flattenBlock (LineBlock lns) = [Block $ Para x | x <- lns]
          flattenBlock CodeBlock{} = []
          flattenBlock RawBlock{} = []
          flattenBlock (BlockQuote blks) = blks
          flattenBlock (OrderedList _ blksList) = concat blksList
          flattenBlock (BulletList blksList) = concat blksList
          flattenBlock (DefinitionList defs) = concat [Block (Para ils) : concat blks | (ils, blks) <- defs]
          flattenBlock (Header _ _ ils) = [Block $ Para ils]
          flattenBlock HorizontalRule = []
          flattenBlock (Table _ capt _ hd bd ft) = flattenCaption capt <>
                                                   flattenTableHead hd <>
                                                   concatMap flattenTableBody bd <>
                                                   flattenTableFoot ft
          flattenBlock (Div _ blks) = blks
          flattenBlock Null = []

          flattenCaption (Caption Nothing body)    = body
          flattenCaption (Caption (Just ils) body) = Block (Para ils) : body

          flattenTableHead (TableHead _ body) = flattenRows body
          flattenTableBody (TableBody _ _ hd bd) = flattenRows hd <> flattenRows bd
          flattenTableFoot (TableFoot _ body) = flattenRows body

          flattenRows = concatMap flattenRow
          flattenRow (Row _ body) = concatMap flattenCell body
          flattenCell (Cell _ _ _ _ blks) = blks

shrinkList :: Arbitrary (Many a) => [a] -> [[a]]
shrinkList = fmap toList . shrink . fromList

shrinkLists :: Arbitrary (Many a) => [[a]] -> [[[a]]]
shrinkLists = fmap (fmap toList) . shrink . fmap fromList

instance Arbitrary Inline where
  arbitrary = resize 3 $ arbInline 2
  shrink = fmap Inline . shrinkInline . unInline

shrinkInline :: ( Arbitrary b, Arbitrary (Many b)
                , Arbitrary i, Arbitrary (Many i))
             => InlineF b i -> [InlineF b i]
shrinkInline (Str s) = Str <$> shrinkText s
shrinkInline (Emph ils) = Emph <$> shrinkList ils
shrinkInline (Underline ils) = Underline <$> shrinkList ils
shrinkInline (Strong ils) = Strong <$> shrinkList ils
shrinkInline (Strikeout ils) = Strikeout <$> shrinkList ils
shrinkInline (Superscript ils) = Superscript <$> shrinkList ils
shrinkInline (Subscript ils) = Subscript <$> shrinkList ils
shrinkInline (SmallCaps ils) = SmallCaps <$> shrinkList ils
shrinkInline (Quoted qtype ils) = Quoted qtype <$> shrinkList ils
shrinkInline (Cite cits ils) = (Cite cits <$> shrinkList ils)
                            ++ (flip Cite ils <$> QC.shrinkList shrinkCitation cits)
shrinkInline (Code attr s) = (Code attr <$> shrinkText s)
                          ++ (flip Code s <$> shrinkAttr attr)
shrinkInline Space = []
shrinkInline SoftBreak = []
shrinkInline LineBreak = []
shrinkInline (Math mtype s) = Math mtype <$> shrinkText s
shrinkInline (RawInline fmt s) = RawInline fmt <$> shrinkText s
shrinkInline (Link attr ils target) = [Link attr ils' target | ils' <- shrinkList ils]
                                   ++ [Link attr ils target' | target' <- shrinkText2 target]
                                   ++ [Link attr' ils target | attr' <- shrinkAttr attr]
shrinkInline (Image attr ils target) = [Image attr ils' target | ils' <- shrinkList ils]
                                    ++ [Image attr ils target' | target' <- shrinkText2 target]
                                    ++ [Image attr' ils target | attr' <- shrinkAttr attr]
shrinkInline (Note blks) = Note <$> shrinkList blks
shrinkInline (Span attr s) = (Span attr <$> shrink s)
                          ++ (flip Span s <$> shrinkAttr attr)

arbInlines :: Int -> Gen [Inline]
arbInlines n = listOf1 (arbInline n) `suchThat` (not . startsWithSpace . fmap unInline)
  where startsWithSpace (Space:_)     = True
        startsWithSpace (SoftBreak:_) = True
        -- Note: no LineBreak, similarly to Text.Pandoc.Builder (trimInlines)
        startsWithSpace _             = False

arbInline :: Int -> Gen Inline
arbInline = fmap Inline . arbInline' arbBlock arbInlines

-- restrict to 3 levels of nesting max; otherwise we get
-- bogged down in indefinitely large structures
arbInline' :: Arbitrary inline
           => (Int -> Gen block) -> (Int -> Gen [inline])
           -> Int -> Gen (InlineF block inline)
arbInline' arbB arbIs n = frequency $
                          [ (60, Str <$> realString)
                          , (40, pure Space)
                          , (10, pure SoftBreak)
                          , (10, pure LineBreak)
                          , (10, Code <$> arbAttr <*> realString)
                          , (5,  elements [ RawInline (Format "html") "<a id=\"eek\">"
                                          , RawInline (Format "latex") "\\my{command}" ])
                          ] ++ [ x | n > 1, x <- nesters]
   where nesters = [ (10, Emph <$> arbIs (n-1))
                   , (10, Underline <$> arbIs (n-1))
                   , (10, Strong <$> arbIs (n-1))
                   , (10, Strikeout <$> arbIs (n-1))
                   , (10, Superscript <$> arbIs (n-1))
                   , (10, Subscript <$> arbIs (n-1))
                   , (10, SmallCaps <$> arbIs (n-1))
                   , (10, Span <$> arbAttr <*> arbIs (n-1))
                   , (10, Quoted <$> arbitrary <*> arbIs (n-1))
                   , (10, Math <$> arbitrary <*> realString)
                   , (10, Link <$> arbAttr <*> arbIs (n-1) <*> ((,) <$> realString <*> realString))
                   , (10, Image <$> arbAttr <*> arbIs (n-1) <*> ((,) <$> realString <*> realString))
                   , (2,  Cite <$> listOf (arbitraryCitation $ arbIs 1) <*> arbIs 1)
                   , (2,  Note <$> resize 3 (listOf1 $ arbB (n-1)))
                   ]

instance Arbitrary Block where
  arbitrary = resize 3 $ arbBlock 2
  shrink = fmap Block . shrinkBlock . unBlock

shrinkBlock :: ( Arbitrary b, Arbitrary (Many b)
               , Arbitrary i, Arbitrary (Many i))
             => BlockF i b -> [BlockF i b]
shrinkBlock (Plain ils) = Plain <$> shrinkList ils
shrinkBlock (Para ils) = Para <$> shrinkList ils
shrinkBlock (LineBlock lns) = LineBlock <$> shrinkLists lns
shrinkBlock (CodeBlock attr s) = (CodeBlock attr <$> shrinkText s)
                         ++ (flip CodeBlock s <$> shrinkAttr attr)
shrinkBlock (RawBlock fmt s) = RawBlock fmt <$> shrinkText s
shrinkBlock (BlockQuote blks) = BlockQuote <$> shrinkList blks
shrinkBlock (OrderedList listAttrs blksList) = OrderedList listAttrs <$> shrinkLists blksList
shrinkBlock (BulletList blksList) = BulletList <$> shrinkLists blksList
shrinkBlock (DefinitionList defs) = DefinitionList <$> shrinkDefinitionList defs
  where shrinkDefinition (ils, blksList) = [(ils', blksList) | ils' <- shrinkList ils]
                                        ++ [(ils, blksList') | blksList' <- shrinkLists blksList]
        shrinkDefinitionList (x:xs) = [xs]
                                   ++ [x':xs | x' <- shrinkDefinition x]
                                   ++ [x:xs' | xs' <- shrinkDefinitionList xs]
        shrinkDefinitionList [] = []
shrinkBlock (Header n attr ils) = (Header n attr <$> shrinkList ils)
                          ++ (flip (Header n) ils <$> shrinkAttr attr)
shrinkBlock HorizontalRule = []
shrinkBlock (Table attr capt specs thead tbody tfoot) =
  -- TODO: shrink number of columns
  [Table attr' capt specs thead tbody tfoot | attr' <- shrinkAttr attr] ++
  [Table attr capt specs thead' tbody tfoot | thead' <- shrinkTableHead thead] ++
  [Table attr capt specs thead tbody' tfoot | tbody' <- QC.shrinkList shrinkTableBody tbody] ++
  [Table attr capt specs thead tbody tfoot' | tfoot' <- shrinkTableFoot tfoot] ++
  [Table attr capt' specs thead tbody tfoot | capt' <- shrinkCaption capt]
shrinkBlock (Div attr blks) = (Div attr <$> shrinkList blks)
                      ++ (flip Div blks <$> shrinkAttr attr)
shrinkBlock Null = []

arbBlock :: Int -> Gen Block
arbBlock = fmap Block . arbBlock' arbInline arbInlines arbBlock

arbBlock' :: (Int -> Gen inline) -> (Int -> Gen [inline])
          -> (Int -> Gen block)
          -> Int -> Gen (BlockF inline block)
arbBlock' arbI arbIs arbB n = frequency $
                         [ (10, Plain <$> arbIs (n-1))
                         , (15, Para <$> arbIs (n-1))
                         , (5,  CodeBlock <$> arbAttr <*> realString)
                         , (3,  LineBlock <$>
                                ((:) <$>
                                  arbIs ((n - 1) `mod` 3) <*>
                                  forM [1..((n - 1) `div` 3)] (const (arbIs 3))))
                         , (2,  elements [ RawBlock (Format "html")
                                            "<div>\n*&amp;*\n</div>"
                                         , RawBlock (Format "latex")
                                            "\\begin[opt]{env}\nhi\n{\\end{env}"
                                         ])
                         , (5,  Header <$> choose (1 :: Int, 6)
                                       <*> pure nullAttr
                                       <*> arbIs (n-1))
                         , (2,  pure HorizontalRule)
                         ] ++ [x | n > 0, x <- nesters]
   where nesters = [ (5, BlockQuote <$> listOf1 (arbB (n-1)))
                   , (5, OrderedList <$> ((,,) <$> (arbitrary `suchThat` (> 0))
                                                <*> arbitrary
                                                <*> arbitrary)
                                      <*> listOf1 (listOf1 $ arbB (n-1)))
                   , (5, BulletList <$> listOf1 (listOf1 $ arbB (n-1)))
                   , (5, DefinitionList <$> listOf1 ((,) <$> arbIs (n-1)
                                                          <*> listOf1 (listOf1 $ arbB (n-1))))
                   , (5, Div <$> arbAttr <*> listOf1 (arbB (n-1)))
                   , (2, do cs <- choose (1 :: Int, 6)
                            bs <- choose (0 :: Int, 2)
                            Table <$> arbAttr
                                  <*> arbitraryCaption' (sized arbI) (sized arbB)
                                  <*> vectorOf cs ((,) <$> arbitrary
                                                       <*> elements [ ColWidthDefault
                                                                    , ColWidth (1/3)
                                                                    , ColWidth 0.25 ])
                                  <*> arbTableHead' arbB (n-1)
                                  <*> vectorOf bs (arbTableBody' arbB (n-1))
                                  <*> arbTableFoot' arbB (n-1))
                   ]

arbRow :: Int -> Gen (RowF Block)
arbRow = arbRow' arbBlock

arbTableHead :: Int -> Gen (TableHeadF Block)
arbTableHead = arbTableHead' arbBlock

arbTableBody :: Int -> Gen (TableBodyF Block)
arbTableBody = arbTableBody' arbBlock

arbTableFoot :: Int -> Gen (TableFootF Block)
arbTableFoot = arbTableFoot' arbBlock

arbCell :: Int -> Gen (CellF Block)
arbCell = arbCell' arbBlock

arbRow' :: (Int -> Gen block) -> Int -> Gen (RowF block)
arbRow' arbB n = do
  cs <- choose (0, 5)
  Row <$> arbAttr <*> vectorOf cs (arbCell' arbB n)

arbTableHead' :: (Int -> Gen block) -> Int -> Gen (TableHeadF block)
arbTableHead' arbB n = do
  rs <- choose (0, 5)
  TableHead <$> arbAttr <*> vectorOf rs (arbRow' arbB n)

arbTableBody' :: (Int -> Gen block) -> Int -> Gen (TableBodyF block)
arbTableBody' arbB n = do
  hrs <- choose (0 :: Int, 2)
  rs <- choose (0, 5)
  rhc <- choose (0, 5)
  TableBody <$> arbAttr
            <*> pure (RowHeadColumns rhc)
            <*> vectorOf hrs (arbRow' arbB n)
            <*> vectorOf rs (arbRow' arbB n)

arbTableFoot' :: (Int -> Gen block) -> Int -> Gen (TableFootF block)
arbTableFoot' arbB n = do
    rs <- choose (0, 5)
    TableFoot <$> arbAttr <*> vectorOf rs (arbRow' arbB n)

arbCell' :: (Int -> Gen block) -> Int -> Gen (CellF block)
arbCell' arbB n = Cell
                 <$> arbAttr
                 <*> arbitrary
                 <*> (RowSpan <$> choose (1 :: Int, 2))
                 <*> (ColSpan <$> choose (1 :: Int, 2))
                 <*> listOf (arbB n)

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
        arbitrary = arbitraryCitation $ arbInlines 1
        shrink = shrinkCitation

arbitraryCitation :: Gen [inline] -> Gen (CitationF inline)
arbitraryCitation arbIs
          = Citation <$> fmap T.pack (listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_'])
                     <*> arbIs
                     <*> arbIs
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

shrinkCitation :: CitationF inline -> [CitationF inline]
shrinkCitation _ = []

instance Arbitrary Row where
  arbitrary = resize 3 $ arbRow 2
  shrink = shrinkRow

shrinkRow :: Arbitrary (Many block) => RowF block -> [RowF block]
shrinkRow (Row attr body)
    = [Row attr' body | attr' <- shrinkAttr attr] ++
      [Row attr body' | body' <- QC.shrinkList shrinkCell body]

instance Arbitrary TableHead where
  arbitrary = resize 3 $ arbTableHead 2
  shrink = shrinkTableHead

shrinkTableHead :: Arbitrary (Many b) => TableHeadF b -> [TableHeadF b]
shrinkTableHead (TableHead attr body)
    = [TableHead attr' body | attr' <- shrinkAttr attr] ++
      [TableHead attr body' | body' <- QC.shrinkList shrinkRow body]

instance Arbitrary TableBody where
  arbitrary = resize 3 $ arbTableBody 2
  shrink = shrinkTableBody

-- TODO: shrink rhc?
shrinkTableBody :: Arbitrary (Many b) => TableBodyF b -> [TableBodyF b]
shrinkTableBody (TableBody attr rhc hd bd)
    = [TableBody attr' rhc hd bd | attr' <- shrinkAttr attr] ++
      [TableBody attr rhc hd' bd | hd' <- QC.shrinkList shrinkRow hd] ++
      [TableBody attr rhc hd bd' | bd' <- QC.shrinkList shrinkRow bd]

instance Arbitrary TableFoot where
  arbitrary = resize 3 $ arbTableFoot 2
  shrink = shrinkTableFoot

shrinkTableFoot :: Arbitrary (Many b) => TableFootF b -> [TableFootF b]
shrinkTableFoot (TableFoot attr body)
    = [TableFoot attr' body | attr' <- shrinkAttr attr] ++
      [TableFoot attr body' | body' <- QC.shrinkList shrinkRow body]

instance Arbitrary Cell where
  arbitrary = resize 3 $ arbCell 2
  shrink = shrinkCell

shrinkCell :: Arbitrary (Many b) => CellF b -> [CellF b]
shrinkCell (Cell attr malign h w body)
    = [Cell attr malign h w body' | body' <- shrinkList body] ++
      [Cell attr' malign h w body | attr' <- shrinkAttr attr] ++
      [Cell attr malign' h w body | malign' <- shrink malign]

instance Arbitrary Caption where
  arbitrary = arbitraryCaption' arbitrary arbitrary
  shrink = shrinkCaption

arbitraryCaption' :: Gen inline -> Gen block -> Gen (CaptionF inline block)
arbitraryCaption' arbI arbB = Caption <$> liftArbitrary (liftArbitrary arbI) <*> liftArbitrary arbB

shrinkCaption :: (Arbitrary inline, Arbitrary (Many block))
              => CaptionF inline block -> [CaptionF inline block]
shrinkCaption (Caption mshort body)
    = [Caption mshort' body | mshort' <- shrink mshort] ++
      [Caption mshort body' | body' <- shrinkList body]

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
