module Networkhs.GraphSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck
--import Control.Exception (evaluate)
import qualified Networkhs.Graph as G

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
	describe "Fundamental Graph structure tests" $ do
		it "should create an empty graph" $ do
			length G.nodes g `shouldBe` 0
				where g = G.newGraph []

