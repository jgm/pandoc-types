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
import qualified Text.Pandoc.Definition.Functors as F
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
          flattenBlock (Table _ capt _ hd bd ft) = flattenCaption capt <>
                                                   flattenTableHead hd <>
                                                   concatMap flattenTableBody bd <>
                                                   flattenTableFoot ft
          flattenBlock (Div _ blks) = blks
          flattenBlock Null = []

          flattenCaption (Caption Nothing body)    = body
          flattenCaption (Caption (Just ils) body) = Para ils : body

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
             => F.Inline b i -> [F.Inline b i]
shrinkInline (F.Str s) = F.Str <$> shrinkText s
shrinkInline (F.Emph ils) = F.Emph <$> shrinkList ils
shrinkInline (F.Underline ils) = F.Underline <$> shrinkList ils
shrinkInline (F.Strong ils) = F.Strong <$> shrinkList ils
shrinkInline (F.Strikeout ils) = F.Strikeout <$> shrinkList ils
shrinkInline (F.Superscript ils) = F.Superscript <$> shrinkList ils
shrinkInline (F.Subscript ils) = F.Subscript <$> shrinkList ils
shrinkInline (F.SmallCaps ils) = F.SmallCaps <$> shrinkList ils
shrinkInline (F.Quoted qtype ils) = F.Quoted qtype <$> shrinkList ils
shrinkInline (F.Cite cits ils) = (F.Cite cits <$> shrinkList ils)
                              ++ (flip F.Cite ils <$> QC.shrinkList shrinkCitation cits)
shrinkInline (F.Code attr s) = (F.Code attr <$> shrinkText s)
                            ++ (flip F.Code s <$> shrinkAttr attr)
shrinkInline F.Space = []
shrinkInline F.SoftBreak = []
shrinkInline F.LineBreak = []
shrinkInline (F.Math mtype s) = F.Math mtype <$> shrinkText s
shrinkInline (F.RawInline fmt s) = F.RawInline fmt <$> shrinkText s
shrinkInline (F.Link attr ils target) = [F.Link attr ils' target | ils' <- shrinkList ils]
                                     ++ [F.Link attr ils target' | target' <- shrinkText2 target]
                                     ++ [F.Link attr' ils target | attr' <- shrinkAttr attr]
shrinkInline (F.Image attr ils target) = [F.Image attr ils' target | ils' <- shrinkList ils]
                                      ++ [F.Image attr ils target' | target' <- shrinkText2 target]
                                      ++ [F.Image attr' ils target | attr' <- shrinkAttr attr]
shrinkInline (F.Note blks) = F.Note <$> shrinkList blks
shrinkInline (F.Span attr s) = (F.Span attr <$> shrink s)
                            ++ (flip F.Span s <$> shrinkAttr attr)

arbInlines :: Int -> Gen [Inline]
arbInlines n = listOf1 (arbInline n) `suchThat` (not . startsWithSpace)
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
           -> Int -> Gen (F.Inline block inline)
arbInline' arbB arbIs n = frequency $
                          [ (60, F.Str <$> realString)
                          , (40, pure F.Space)
                          , (10, pure F.SoftBreak)
                          , (10, pure F.LineBreak)
                          , (10, F.Code <$> arbAttr <*> realString)
                          , (5,  elements [ F.RawInline (Format "html") "<a id=\"eek\">"
                                          , F.RawInline (Format "latex") "\\my{command}" ])
                          ] ++ [ x | n > 1, x <- nesters]
   where nesters = [ (10, F.Emph <$> arbIs (n-1))
                   , (10, F.Underline <$> arbIs (n-1))
                   , (10, F.Strong <$> arbIs (n-1))
                   , (10, F.Strikeout <$> arbIs (n-1))
                   , (10, F.Superscript <$> arbIs (n-1))
                   , (10, F.Subscript <$> arbIs (n-1))
                   , (10, F.SmallCaps <$> arbIs (n-1))
                   , (10, F.Span <$> arbAttr <*> arbIs (n-1))
                   , (10, F.Quoted <$> arbitrary <*> arbIs (n-1))
                   , (10, F.Math <$> arbitrary <*> realString)
                   , (10, F.Link <$> arbAttr <*> arbIs (n-1) <*> ((,) <$> realString <*> realString))
                   , (10, F.Image <$> arbAttr <*> arbIs (n-1) <*> ((,) <$> realString <*> realString))
                   , (2,  F.Cite <$> listOf (arbitraryCitation $ arbIs 1) <*> arbIs 1)
                   , (2,  F.Note <$> resize 3 (listOf1 $ arbB (n-1)))
                   ]

instance Arbitrary Block where
  arbitrary = resize 3 $ arbBlock 2
  shrink = fmap Block . shrinkBlock . unBlock

shrinkBlock :: ( Arbitrary b, Arbitrary (Many b)
               , Arbitrary i, Arbitrary (Many i))
            => F.Block i b -> [F.Block i b]
shrinkBlock (F.Plain ils) = F.Plain <$> shrinkList ils
shrinkBlock (F.Para ils) = F.Para <$> shrinkList ils
shrinkBlock (F.LineBlock lns) = F.LineBlock <$> shrinkLists lns
shrinkBlock (F.CodeBlock attr s) = (F.CodeBlock attr <$> shrinkText s)
                           ++ (flip F.CodeBlock s <$> shrinkAttr attr)
shrinkBlock (F.RawBlock fmt s) = F.RawBlock fmt <$> shrinkText s
shrinkBlock (F.BlockQuote blks) = F.BlockQuote <$> shrinkList blks
shrinkBlock (F.OrderedList listAttrs blksList) = F.OrderedList listAttrs <$> shrinkLists blksList
shrinkBlock (F.BulletList blksList) = F.BulletList <$> shrinkLists blksList
shrinkBlock (F.DefinitionList defs) = F.DefinitionList <$> shrinkDefinitionList defs
  where shrinkDefinition (ils, blksList) = [(ils', blksList) | ils' <- shrinkList ils]
                                        ++ [(ils, blksList') | blksList' <- shrinkLists blksList]
        shrinkDefinitionList (x:xs) = [xs]
                                   ++ [x':xs | x' <- shrinkDefinition x]
                                   ++ [x:xs' | xs' <- shrinkDefinitionList xs]
        shrinkDefinitionList [] = []
shrinkBlock (F.Header n attr ils) = (F.Header n attr <$> shrinkList ils)
                           ++ (flip (F.Header n) ils <$> shrinkAttr attr)
shrinkBlock F.HorizontalRule = []
shrinkBlock (F.Table attr capt specs thead tbody tfoot) =
  -- F.TODO: shrink number of columns
  [F.Table attr' capt specs thead tbody tfoot | attr' <- shrinkAttr attr] ++
  [F.Table attr capt specs thead' tbody tfoot | thead' <- shrinkTableHead thead] ++
  [F.Table attr capt specs thead tbody' tfoot | tbody' <- QC.shrinkList shrinkTableBody tbody] ++
  [F.Table attr capt specs thead tbody tfoot' | tfoot' <- shrinkTableFoot tfoot] ++
  [F.Table attr capt' specs thead tbody tfoot | capt' <- shrinkCaption capt]
shrinkBlock (F.Div attr blks) = (F.Div attr <$> shrinkList blks)
                        ++ (flip F.Div blks <$> shrinkAttr attr)
shrinkBlock F.Null = []

arbBlock :: Int -> Gen Block
arbBlock = fmap Block . arbBlock' arbInline arbInlines arbBlock

arbBlock' :: (Int -> Gen inline) -> (Int -> Gen [inline])
          -> (Int -> Gen block)
          -> Int -> Gen (F.Block inline block)
arbBlock' arbI arbIs arbB n = frequency $
                         [ (10, F.Plain <$> arbIs (n-1))
                         , (15, F.Para <$> arbIs (n-1))
                         , (5,  F.CodeBlock <$> arbAttr <*> realString)
                         , (3,  F.LineBlock <$>
                                  ((:) <$>
                                    arbIs ((n - 1) `mod` 3) <*>
                                    forM [1..((n - 1) `div` 3)] (const (arbIs 3))))
                         , (2,  elements [ F.RawBlock (F.Format "html")
                                            "<div>\n*&amp;*\n</div>"
                                         , F.RawBlock (F.Format "latex")
                                            "\\begin[opt]{env}\nhi\n{\\end{env}"
                                         ])
                         , (5,  F.Header <$> choose (1 :: Int, 6)
                                         <*> pure nullAttr
                                         <*> arbIs (n-1))
                         , (2,  pure F.HorizontalRule)
                         ] ++ [x | n > 0, x <- nesters]
   where nesters = [ (5, F.BlockQuote <$> listOf1 (arbB (n-1)))
                   , (5, F.OrderedList <$> ((,,) <$> (arbitrary `suchThat` (> 0))
                                                 <*> arbitrary
                                                 <*> arbitrary)
                                      <*> listOf1 (listOf1 $ arbB (n-1)))
                   , (5, F.BulletList <$> listOf1 (listOf1 $ arbB (n-1)))
                   , (5, F.DefinitionList <$> listOf1 ((,) <$> arbIs (n-1)
                                                           <*> listOf1 (listOf1 $ arbB (n-1))))
                   , (5, F.Div <$> arbAttr <*> listOf1 (arbB (n-1)))
                   , (2, do cs <- choose (1 :: Int, 6)
                            bs <- choose (0 :: Int, 2)
                            F.Table <$> arbAttr
                                    <*> arbitraryCaption' (sized arbI) (sized arbB)
                                    <*> vectorOf cs ((,) <$> arbitrary
                                                         <*> elements [ F.ColWidthDefault
                                                                      , F.ColWidth (1/3)
                                                                      , F.ColWidth 0.25 ])
                                    <*> arbTableHead' arbB (n-1)
                                    <*> vectorOf bs (arbTableBody' arbB (n-1))
                                    <*> arbTableFoot' arbB (n-1))
                   ]

arbRow :: Int -> Gen (F.Row Block)
arbRow = arbRow' arbBlock

arbTableHead :: Int -> Gen (F.TableHead Block)
arbTableHead = arbTableHead' arbBlock

arbTableBody :: Int -> Gen (F.TableBody Block)
arbTableBody = arbTableBody' arbBlock

arbTableFoot :: Int -> Gen (F.TableFoot Block)
arbTableFoot = arbTableFoot' arbBlock

arbCell :: Int -> Gen (F.Cell Block)
arbCell = arbCell' arbBlock

arbRow' :: (Int -> Gen block) -> Int -> Gen (F.Row block)
arbRow' arbB n = do
  cs <- choose (0, 5)
  Row <$> arbAttr <*> vectorOf cs (arbCell' arbB n)

arbTableHead' :: (Int -> Gen block) -> Int -> Gen (F.TableHead block)
arbTableHead' arbB n = do
  rs <- choose (0, 5)
  TableHead <$> arbAttr <*> vectorOf rs (arbRow' arbB n)

arbTableBody' :: (Int -> Gen block) -> Int -> Gen (F.TableBody block)
arbTableBody' arbB n = do
  hrs <- choose (0 :: Int, 2)
  rs <- choose (0, 5)
  rhc <- choose (0, 5)
  TableBody <$> arbAttr
            <*> pure (RowHeadColumns rhc)
            <*> vectorOf hrs (arbRow' arbB n)
            <*> vectorOf rs (arbRow' arbB n)

arbTableFoot' :: (Int -> Gen block) -> Int -> Gen (F.TableFoot block)
arbTableFoot' arbB n = do
    rs <- choose (0, 5)
    TableFoot <$> arbAttr <*> vectorOf rs (arbRow' arbB n)

arbCell' :: (Int -> Gen block) -> Int -> Gen (F.Cell block)
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

arbitraryCitation :: Gen [inline] -> Gen (F.Citation inline)
arbitraryCitation arbIs
          = Citation <$> fmap T.pack (listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_'])
                     <*> arbIs
                     <*> arbIs
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

shrinkCitation :: F.Citation inline -> [F.Citation inline]
shrinkCitation _ = []

instance Arbitrary Row where
  arbitrary = resize 3 $ arbRow 2
  shrink = shrinkRow

shrinkRow :: Arbitrary (Many block) => F.Row block -> [F.Row block]
shrinkRow (Row attr body)
    = [Row attr' body | attr' <- shrinkAttr attr] ++
      [Row attr body' | body' <- QC.shrinkList shrinkCell body]

instance Arbitrary TableHead where
  arbitrary = resize 3 $ arbTableHead 2
  shrink = shrinkTableHead

shrinkTableHead :: Arbitrary (Many b) => F.TableHead b -> [F.TableHead b]
shrinkTableHead (TableHead attr body)
    = [TableHead attr' body | attr' <- shrinkAttr attr] ++
      [TableHead attr body' | body' <- QC.shrinkList shrinkRow body]

instance Arbitrary TableBody where
  arbitrary = resize 3 $ arbTableBody 2
  shrink = shrinkTableBody

-- TODO: shrink rhc?
shrinkTableBody :: Arbitrary (Many b) => F.TableBody b -> [F.TableBody b]
shrinkTableBody (TableBody attr rhc hd bd)
    = [TableBody attr' rhc hd bd | attr' <- shrinkAttr attr] ++
      [TableBody attr rhc hd' bd | hd' <- QC.shrinkList shrinkRow hd] ++
      [TableBody attr rhc hd bd' | bd' <- QC.shrinkList shrinkRow bd]

instance Arbitrary TableFoot where
  arbitrary = resize 3 $ arbTableFoot 2
  shrink = shrinkTableFoot

shrinkTableFoot :: Arbitrary (Many b) => F.TableFoot b -> [F.TableFoot b]
shrinkTableFoot (TableFoot attr body)
    = [TableFoot attr' body | attr' <- shrinkAttr attr] ++
      [TableFoot attr body' | body' <- QC.shrinkList shrinkRow body]

instance Arbitrary Cell where
  arbitrary = resize 3 $ arbCell 2
  shrink = shrinkCell

shrinkCell :: Arbitrary (Many b) => F.Cell b -> [F.Cell b]
shrinkCell (Cell attr malign h w body)
    = [Cell attr malign h w body' | body' <- shrinkList body] ++
      [Cell attr' malign h w body | attr' <- shrinkAttr attr] ++
      [Cell attr malign' h w body | malign' <- shrink malign]

instance Arbitrary Caption where
  arbitrary = arbitraryCaption' arbitrary arbitrary
  shrink = shrinkCaption

arbitraryCaption' :: Gen inline -> Gen block -> Gen (F.Caption inline block)
arbitraryCaption' arbI arbB = Caption <$> liftArbitrary (liftArbitrary arbI) <*> liftArbitrary arbB

shrinkCaption :: (Arbitrary inline, Arbitrary (Many block))
              => F.Caption inline block -> [F.Caption inline block]
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
