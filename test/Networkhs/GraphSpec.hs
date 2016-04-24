module Networkhs.GraphSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import qualified Networkhs.Graph as G

main :: IO ()
main = hspec spec

graph1 :: G.Graph [Char]
graph1 = let{
		xs = ["beijing","osaka","bangkok","moscow"];
		ns = map (\n -> G.Node n "city") xs;
	}
	in G.newGraph ns

spec :: Spec
spec = do
	describe "Fundamental Graph structure tests" $ do
		it "should create an empty graph" $ do
			let {g = G.newGraph []; len = length $ G.nodes g }
				in len `shouldBe` 0

		it "should create a graph with nodes" $ do
			let {
				a = G.Node "brasil" Nothing;
				b = G.Node "australia" Nothing;
				c = G.Node "japan" Nothing;
				g = G.newGraph [a,b,c]
			}
				in map (G.key) (G.nodes g) `shouldBe` ["brasil","australia","japan"]

		it "should add new node to a graph" $ do
			let {
				g = G.addNode ("singapore","") graph1;
			}
				in map (G.key) (G.nodes g) `shouldBe` ["beijing","osaka","bangkok","moscow","singapore"]

