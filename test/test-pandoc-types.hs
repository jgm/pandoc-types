import Text.Pandoc.Definition
import Test.QuickCheck
import Text.Pandoc.Arbitrary ()
import Data.Aeson

prop_roundtrip :: Pandoc -> Bool
prop_roundtrip doc = case decode $ encode doc :: (Maybe Pandoc) of
  Just doc' -> doc == doc'
  _         -> False

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 100 }  prop_roundtrip


