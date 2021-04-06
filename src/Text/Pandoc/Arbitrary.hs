{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}
-- provides Arbitrary instance for Pandoc types
module Text.Pandoc.Arbitrary ()
where
import Test.QuickCheck
import Control.Applicative (Applicative ((<*>), pure), (<$>))
import Control.Monad (forM)
import qualified Data.Map as M
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

instance Arbitrary Attr where
  arbitrary = do
    id' <- elements ["","loc"]
    classes' <- elements [[],["haskell"],["c","numberLines"]]
    keyvals <- elements [[],[("start","22")],[("a","11"),("b_2","a b c")]]
    return $ Attr id' classes' (M.fromList keyvals)
  shrink (Attr a b c)
    = [ Attr a' b c | a' <- shrinkText a]
   ++ [ Attr a b' c | b' <- liftShrink shrinkText b]
   ++ [ Attr a b (M.fromList c') | c' <- liftShrink shrinkText2 (M.toList c) ]

instance Arbitrary Inlines where
  arbitrary = (fromList :: [Inline] -> Inlines) <$> arbitrary
  shrink = fmap fromList . ((++) <$> shrink <*> flattenShrinkInlines) . toList
    where flattenShrinkInlines (x:xs) =
            let x' = flattenInline x
            in (if null x' then [] else [x' ++ xs]) ++ [x:xs' | xs' <- flattenShrinkInlines xs]
          flattenShrinkInlines [] = []
          flattenInline :: Inline -> [Inline]
          flattenInline (Str _) = []
          flattenInline (Emph _ ils) = ils
          flattenInline (Underline _ ils) = ils
          flattenInline (Strong _ ils) = ils
          flattenInline (Strikeout _ ils) = ils
          flattenInline (Superscript _ ils) = ils
          flattenInline (Subscript _ ils) = ils
          flattenInline (SmallCaps _ ils) = ils
          flattenInline (Quoted _ _ ils) = ils
          flattenInline (Cite _ _ ils) = ils
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
          flattenBlock (LineBlock a lns) = [Para a x | x <- lns]
          flattenBlock CodeBlock{} = []
          flattenBlock RawBlock{} = []
          flattenBlock (BlockQuote _ blks) = blks
          flattenBlock (OrderedList _ _ blksList) = concat blksList
          flattenBlock (BulletList _ blksList) = concat blksList
          flattenBlock (DefinitionList a defs) = concat [Para a ils:concat blks | (ils, blks) <- defs]
          flattenBlock (Header _ a ils) = [Para a ils]
          flattenBlock HorizontalRule{} = []
          flattenBlock (Table _ capt _ hd bd ft) = flattenCaption capt <>
                                                   flattenTableHead hd <>
                                                   concatMap flattenTableBody bd <>
                                                   flattenTableFoot ft
          flattenBlock (Div _ blks) = blks
          flattenBlock Null = []

          flattenCaption (Caption _ Nothing body)    = body
          flattenCaption (Caption a (Just ils) body) = Para a ils : body

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
  shrink (Emph attr ils) = [Emph attr ils' | ils' <- shrinkInlineList ils]
                        ++ [Emph attr' ils | attr' <- shrink attr]
  shrink (Underline attr ils) = [Underline attr ils' | ils' <- shrinkInlineList ils]
                             ++ [Underline attr' ils | attr' <- shrink attr]
  shrink (Strong attr ils) = [Strong attr ils' | ils' <- shrinkInlineList ils]
                          ++ [Strong attr' ils | attr' <- shrink attr]
  shrink (Strikeout attr ils) = [Strikeout attr ils' | ils' <- shrinkInlineList ils]
                             ++ [Strikeout attr' ils | attr' <- shrink attr]
  shrink (Superscript attr ils) = [Superscript attr ils' | ils' <- shrinkInlineList ils]
                               ++ [Superscript attr' ils | attr' <- shrink attr]
  shrink (Subscript attr ils) = [Subscript attr ils' | ils' <- shrinkInlineList ils]
                             ++ [Subscript attr' ils | attr' <- shrink attr]
  shrink (SmallCaps attr ils) = [SmallCaps attr ils' | ils' <- shrinkInlineList ils]
                             ++ [SmallCaps attr' ils | attr' <- shrink attr]
  shrink (Quoted attr qtype ils) = [Quoted attr qtype ils' | ils' <- shrinkInlineList ils]
                                ++ [Quoted attr' qtype ils | attr' <- shrink attr]
  shrink (Cite attr cits ils) = [Cite attr cits ils' | ils' <- shrinkInlineList ils]
                             ++ [Cite attr cits' ils | cits' <- shrink cits]
                             ++ [Cite attr' cits ils | attr' <- shrink attr]
  shrink (Code attr s) = (Code attr <$> shrinkText s)
                      ++ (flip Code s <$> shrink attr)
  shrink Space = []
  shrink SoftBreak = []
  shrink LineBreak = []
  shrink (Math attr mtype s) = [Math attr mtype s' | s' <- shrinkText s]
                            ++ [Math attr' mtype s | attr' <- shrink attr]
  shrink (RawInline attr fmt s) = [RawInline attr fmt s' | s' <- shrinkText s]
                               ++ [RawInline attr' fmt s | attr' <- shrink attr]
  shrink (Link attr ils target) = [Link attr ils' target | ils' <- shrinkInlineList ils]
                               ++ [Link attr ils target' | target' <- shrinkText2 target]
                               ++ [Link attr' ils target | attr' <- shrink attr]
  shrink (Image attr ils target) = [Image attr ils' target | ils' <- shrinkInlineList ils]
                                ++ [Image attr ils target' | target' <- shrinkText2 target]
                                ++ [Image attr' ils target | attr' <- shrink attr]
  shrink (Note attr ils) = [Note attr ils' | ils' <- shrinkBlockList ils]
                        ++ [Note attr' ils | attr' <- shrink attr]
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
                          , (10, Code <$> arbitrary <*> realString)
                          , (5,  oneof [raw1, raw2])
                          ] ++ [ x | n > 1, x <- nesters]
   where nesters = [ (10, Emph <$> arbitrary <*> arbInlines (n-1))
                   , (10, Underline <$> arbitrary <*> arbInlines (n-1))
                   , (10, Strong <$> arbitrary <*> arbInlines (n-1))
                   , (10, Strikeout <$> arbitrary <*> arbInlines (n-1))
                   , (10, Superscript <$> arbitrary <*> arbInlines (n-1))
                   , (10, Subscript <$> arbitrary <*> arbInlines (n-1))
                   , (10, SmallCaps <$> arbitrary <*> arbInlines (n-1))
                   , (10, Span <$> arbitrary <*> arbInlines (n-1))
                   , (10, Quoted <$> arbitrary <*> arbitrary <*> arbInlines (n-1))
                   , (10, Math <$> arbitrary <*> arbitrary <*> realString)
                   , (10, Link <$> arbitrary <*> arbInlines (n-1) <*> ((,) <$> realString <*> realString))
                   , (10, Image <$> arbitrary <*> arbInlines (n-1) <*> ((,) <$> realString <*> realString))
                   , (2,  Cite <$> arbitrary <*> arbitrary <*> arbInlines 1)
                   , (2,  Note <$> arbitrary <*> resize 3 (listOf1 $ arbBlock (n-1)))
                   ]
         raw1 = (\x -> RawInline x (Format "html") "<a id=\"eek\">") <$> arbitrary
         raw2 = (\x -> RawInline x (Format "latex") "\\my{command}") <$> arbitrary
instance Arbitrary Block where
  arbitrary = resize 3 $ arbBlock 2
  shrink (Plain ils) = Plain <$> shrinkInlineList ils
  shrink (Para attr ils) = (Para attr <$> shrinkInlineList ils)
                        ++ (flip Para ils <$> shrink attr)
  shrink (LineBlock attr lns) = (LineBlock attr <$> shrinkInlinesList lns)
                             ++ (flip LineBlock lns <$> shrink attr)
  shrink (CodeBlock attr s) = (CodeBlock attr <$> shrinkText s)
                           ++ (flip CodeBlock s <$> shrink attr)
  shrink (RawBlock attr fmt s) = [RawBlock attr fmt s' | s' <- shrinkText s]
                              ++ [RawBlock attr' fmt s | attr' <- shrink attr]
  shrink (BlockQuote attr blks) = (BlockQuote attr <$> shrinkBlockList blks)
                               ++ (flip BlockQuote blks <$> shrink attr)
  shrink (OrderedList attr listAttrs blksList)
    = [OrderedList attr listAttrs blksList' | blksList' <- shrinkBlocksList blksList]
   ++ [OrderedList attr' listAttrs blksList | attr' <- shrink attr]
  shrink (BulletList attr blksList) = (BulletList attr <$> shrinkBlocksList blksList)
                                   ++ (flip BulletList blksList <$> shrink attr)
  shrink (DefinitionList attr defs) = (DefinitionList attr <$> shrinkDefinitionList defs)
                                   ++ (flip DefinitionList defs <$> shrink attr)
    where shrinkDefinition (ils, blksList) = [(ils', blksList) | ils' <- shrinkInlineList ils]
                                          ++ [(ils, blksList') | blksList' <- shrinkBlocksList blksList]
          shrinkDefinitionList (x:xs) = [xs]
                                     ++ [x':xs | x' <- shrinkDefinition x]
                                     ++ [x:xs' | xs' <- shrinkDefinitionList xs]
          shrinkDefinitionList [] = []
  shrink (Header n attr ils) = (Header n attr <$> shrinkInlineList ils)
                            ++ (flip (Header n) ils <$> shrink attr)
  shrink (HorizontalRule attr) = HorizontalRule <$> shrink attr
  shrink (Table attr capt specs thead tbody tfoot) =
    -- TODO: shrink number of columns
    [Table attr' capt specs thead tbody tfoot | attr' <- shrink attr] ++
    [Table attr capt specs thead' tbody tfoot | thead' <- shrink thead] ++
    [Table attr capt specs thead tbody' tfoot | tbody' <- shrink tbody] ++
    [Table attr capt specs thead tbody tfoot' | tfoot' <- shrink tfoot] ++
    [Table attr capt' specs thead tbody tfoot | capt' <- shrink capt]
  shrink (Div attr blks) = (Div attr <$> shrinkBlockList blks)
                        ++ (flip Div blks <$> shrink attr)
  shrink Null = []

arbBlock :: Int -> Gen Block
arbBlock n = frequency $ [ (10, Plain <$> arbInlines (n-1))
                         , (15, Para <$> arbitrary <*> arbInlines (n-1))
                         , (5,  CodeBlock <$> arbitrary <*> realString)
                         , (3,  LineBlock <$> arbitrary <*>
                                ((:) <$>
                                  arbInlines ((n - 1) `mod` 3) <*>
                                  forM [1..((n - 1) `div` 3)] (const (arbInlines 3))))
                         , (2,  oneof [raw1, raw2])
                         , (5,  Header <$> choose (1 :: Int, 6)
                                       <*> pure nullAttr
                                       <*> arbInlines (n-1))
                         , (2,  HorizontalRule <$> arbitrary)
                         ] ++ [x | n > 0, x <- nesters]
   where nesters = [ (5, BlockQuote <$> arbitrary <*> listOf1 (arbBlock (n-1)))
                   , (5, OrderedList <$> arbitrary
                                     <*> ((,,) <$> (arbitrary `suchThat` (> 0))
                                               <*> arbitrary
                                               <*> arbitrary)
                                      <*> listOf1 (listOf1 $ arbBlock (n-1)))
                   , (5, BulletList <$> arbitrary <*> listOf1 (listOf1 $ arbBlock (n-1)))
                   , (5, DefinitionList <$> arbitrary
                                        <*> listOf1 ((,) <$> arbInlines (n-1)
                                                         <*> listOf1 (listOf1 $ arbBlock (n-1))))
                   , (5, Div <$> arbitrary <*> listOf1 (arbBlock (n-1)))
                   , (2, do cs <- choose (1 :: Int, 6)
                            bs <- choose (0 :: Int, 2)
                            Table <$> arbitrary
                                  <*> arbitrary
                                  <*> vectorOf cs ((,) <$> arbitrary
                                                       <*> elements [ ColWidthDefault
                                                                    , ColWidth (1/3)
                                                                    , ColWidth 0.25 ])
                                  <*> arbTableHead (n-1)
                                  <*> vectorOf bs (arbTableBody (n-1))
                                  <*> arbTableFoot (n-1))
                   ]
         raw1 = (\x -> RawBlock x (Format "html") "<div>\n*&amp;*\n</div>") <$> arbitrary
         raw2 = (\x -> RawBlock x (Format "latex") "\\begin[opt]{env}\nhi\n{\\end{env}")
                <$> arbitrary

arbRow :: Int -> Gen Row
arbRow n = do
  cs <- choose (0, 5)
  Row <$> arbitrary <*> vectorOf cs (arbCell n)

arbTableHead :: Int -> Gen TableHead
arbTableHead n = do
  rs <- choose (0, 5)
  TableHead <$> arbitrary <*> vectorOf rs (arbRow n)

arbTableBody :: Int -> Gen TableBody
arbTableBody n = do
  hrs <- choose (0 :: Int, 2)
  rs <- choose (0, 5)
  rhc <- choose (0, 5)
  TableBody <$> arbitrary
            <*> pure (RowHeadColumns rhc)
            <*> vectorOf hrs (arbRow n)
            <*> vectorOf rs (arbRow n)

arbTableFoot :: Int -> Gen TableFoot
arbTableFoot n = do
    rs <- choose (0, 5)
    TableFoot <$> arbitrary <*> vectorOf rs (arbRow n)

arbCell :: Int -> Gen Cell
arbCell n = Cell <$> arbitrary
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
    = [Row attr' body | attr' <- shrink attr] ++
      [Row attr body' | body' <- shrink body]

instance Arbitrary TableHead where
  arbitrary = resize 3 $ arbTableHead 2
  shrink (TableHead attr body)
    = [TableHead attr' body | attr' <- shrink attr] ++
      [TableHead attr body' | body' <- shrink body]

instance Arbitrary TableBody where
  arbitrary = resize 3 $ arbTableBody 2
  -- TODO: shrink rhc?
  shrink (TableBody attr rhc hd bd)
    = [TableBody attr' rhc hd bd | attr' <- shrink attr] ++
      [TableBody attr rhc hd' bd | hd' <- shrink hd] ++
      [TableBody attr rhc hd bd' | bd' <- shrink bd]

instance Arbitrary TableFoot where
  arbitrary = resize 3 $ arbTableFoot 2
  shrink (TableFoot attr body)
    = [TableFoot attr' body | attr' <- shrink attr] ++
      [TableFoot attr body' | body' <- shrink body]

instance Arbitrary Cell where
  arbitrary = resize 3 $ arbCell 2
  shrink (Cell attr malign h w body)
    = [Cell attr malign h w body' | body' <- shrinkBlockList body] ++
      [Cell attr' malign h w body | attr' <- shrink attr] ++
      [Cell attr malign' h w body | malign' <- shrink malign]

instance Arbitrary Caption where
  arbitrary = Caption <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (Caption attr mshort body)
    = [Caption attr mshort' body | mshort' <- shrink mshort] ++
      [Caption attr mshort body' | body' <- shrinkBlockList body] ++
      [Caption attr' mshort body | attr' <- shrink attr]

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
