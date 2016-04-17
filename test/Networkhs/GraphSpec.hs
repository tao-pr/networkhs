module Networkhs.GraphSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Networkhs.Graph as G

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
	describe "Fundamental Graph structure tests" $ do
		it "should create an empty graph" $ do
			length G.nodes g `shouldBe` 0
				where g = G.newGraph []

		it "should create a graph with nodes" $ do
			G.nodes g `shouldBe` [a,b,c]
				where {
					a = G.Node "brasil" 100;
					b = G.Node "australia" 200;
					c = G.Node "japan" 300;
					g = G.newGraph [a,b,c]
				}
