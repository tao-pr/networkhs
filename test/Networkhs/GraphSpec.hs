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

		it "should add a link" $ do
			let {
				g  = G.addLink ("beijing","osaka",5.0) graph1;
				g' = G.addLink ("osaka","bangkok",12.5) g;
				l1 = G.link "beijing" "osaka" g';
				l2 = G.link "osaka" "bangkok" g';
				l3 = G.link "osaka" "moscow" g';
			}
				in [l1,l2,l3] `shouldBe` [Just 5.0, Just 12.5, Nothing]

		it "bidirectional link vs oneway link" $ do
			let {
				g  = G.addLink ("beijing","osaka",1.1) graph1;
				g' = G.addBiLink ("bangkok","beijing",2.2) g;
				l1 = G.link "beijing" "osaka" g';
				l2 = G.link "osaka" "beijing" g';
				l3 = G.link "bangkok" "beijing" g';
				l4 = G.link "beijing" "bangkok" g';
			}
				in [l1,l2,l3,l4] `shouldBe` [Just 1.1, Nothing, Just 2.2, Just 2.2]