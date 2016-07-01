module Networkhs.GraphSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck()
import Data.Maybe()
import qualified Networkhs.Graph as G

main :: IO ()
main = hspec spec

-- Simple graph without any links
graph1 :: G.Graph [Char]
graph1 = 
  let { xs = ["beijing","osaka","bangkok","moscow"]
      ; ns = map (\n -> G.Node n "city") xs
      }
  in G.newGraph ns

-- Simple graph with some bidirectional links
--
--  moscow─────────────────────────┐      tokyo
--       ╲            beijing        ╲     /
--         ╲           │    ╲_________osaka
--           ╲        ╱                /
--             bangkok────────────────┘
--
graph2 :: G.Graph [Char]
graph2 = 
  let links = [ ("beijing","osaka",3)
              , ("beijing","bangkok",8)
              , ("moscow","bangkok",15)
              , ("osaka","moscow",10)
              , ("osaka","bangkok",12) ]
  in foldl (G.addBiLink) graph1 links

cyclicGraph1 :: G.Graph [Char]
cyclicGraph1  = 
  let { ns    = ["A","B","C","D"]
      ; ns'   = map (\n -> G.Node n "city") ns
      ; g     = G.newGraph ns'
      ; links = [ ("A","B",1)
                , ("B","C",1)
                , ("C","A",2)
                , ("C","B",1)
                ]
      }
  in foldl (G.addLink) g links

acyclicGraph1 :: G.Graph [Char]
acyclicGraph1 = 
  let { ns    = ["A","B","C","D"]
      ; ns'   = map (\n -> G.Node n "city") ns
      ; g     = G.newGraph ns'
      ; links = [ ("A","B",1)
                , ("B","C",1)
                , ("B","D",2)
                , ("A","D",1)
                ]
      }
  in foldl (G.addLink) g links

hexagon :: G.Graph [Char]
hexagon       = 
  let { ns    = ["A","B","C","D","E","F"]
      ; ns'   = map (\n -> G.Node n "city") ns
      ; g     = G.newGraph ns'
      ; links = [ ("A","B",1) -- Outer edge
                , ("B","C",1) -- Outer edge
                , ("C","D",1) -- Outer edge
                , ("D","E",1) -- Outer edge
                , ("E","F",1) -- Outer edge
                , ("A","C",1)
                , ("A","F",1)
                , ("B","C",1)
                , ("B","D",1)
                , ("B","F",1)
                , ("B","E",1)
                , ("C","E",1)
                , ("D","F",1)
                ]
      }
    in foldl (G.addBiLink) g links

spec :: Spec
spec = do
  describe "Fundamental Graph structure tests" $ do
    it "should create an empty graph" $ do
      let {g = G.newGraph []; len = length $ G.nodes g }
        in len `shouldBe` 0

    it "should create a graph with nodes" $ do
      let { a = G.Node "brasil" Nothing
          ; b = G.Node "australia" Nothing
          ; c = G.Node "japan" Nothing
          ; g = G.newGraph [a,b,c]
      }
        in map (G.key) (G.nodes g) `shouldBe` ["brasil","australia","japan"]

    it "should add new node to a graph" $ do
      let g = G.addNode ("singapore","") graph1
        in map (G.key) (G.nodes g) `shouldBe` ["beijing","osaka","bangkok","moscow","singapore"]

    it "should add a link" $ do
      let { g  = G.addLink graph1 ("beijing","osaka",5.0)
          ; g' = G.addLink g ("osaka","bangkok",12.5)
          ; l1 = G.link "beijing" "osaka" g'
          ; l2 = G.link "osaka" "bangkok" g'
          ; l3 = G.link "osaka" "moscow" g'
      }
        in [l1,l2,l3] `shouldBe` [Just 5.0, Just 12.5, Nothing]

    it "bidirectional link vs oneway link" $ do
      let { g  = G.addLink graph1 ("beijing","osaka",1.1)
          ; g' = G.addBiLink g ("bangkok","beijing",2.2)
          ; l1 = G.link "beijing" "osaka" g'
          ; l2 = G.link "osaka" "beijing" g'
          ; l3 = G.link "bangkok" "beijing" g'
          ; l4 = G.link "beijing" "bangkok" g'
      }
        in [l1,l2,l3,l4] `shouldBe` [Just 1.1, Nothing, Just 2.2, Just 2.2]

    it "should get links back and forth" $ do
      let { g  = G.addLink graph1 ("beijing","osaka",1.1)
          ; g' = G.addBiLink g ("bangkok","beijing",2.2)
          ; l1 = G.linkBackAndForth "beijing" "osaka" g'
          ; l2 = G.linkBackAndForth "beijing" "bangkok" g'
      }
        in [l1,l2] `shouldBe` [(Just 1.1,Nothing),(Just 2.2, Just 2.2)]

    it "should verify a digraph" $ do
      G.isUndirected graph2 `shouldBe` True

    it "should verify an undirected graph" $ do
      let g' = G.addLink graph2 ("Tokyo","Beijing",3.5)
        in G.isUndirected g' `shouldBe` False

    it "should list all edges" $ do
      let { edg  = G.edges graph2
          ; edg0 = [ ("beijing","osaka",3.0)
                   , ("osaka","beijing",3.0)
                   , ("bangkok","beijing",8.0)
                   , ("beijing","bangkok",8.0)
                   , ("moscow","osaka",10.0)
                   , ("osaka","moscow",10.0)
                   , ("bangkok","osaka",12.0)
                   , ("osaka","bangkok",12.0)
                   , ("bangkok","moscow",15.0)
                   , ("moscow","bangkok",15.0)
                   ]
          }
        in edg `shouldBe` edg0

  describe "Fundamental route structure tests" $ do
    it "should create a new route" $ do
      case G.newRoute ["a","b","c","d"] of
        G.Route [] -> error "Route should not be empty"
        G.Route r -> r `shouldBe` [("a","b"),("b","c"),("c","d")]

    it "should compute distance of a single hop" $ do
      let r = G.Route [("beijing","osaka")]
        in G.routeDistance r graph2 `shouldBe` Just 3

    it "should compute total distance" $ do
      let r = G.Route [("beijing","osaka"),("osaka","bangkok"),("bangkok","moscow")]
        in G.routeDistance r graph2 `shouldBe` Just 30

    it "should fail to compute distance of unknown node" $ do
      let r = G.Route [("bangkok","osaka"),("osaka","shanghai")]
        in G.routeDistance r graph2 `shouldSatisfy` null

    it "should fail to compute distance of unconnected route" $ do
      let r = G.newRoute ["beijing","osaka","bangkok","moscow","beijing"]
        in G.routeDistance r graph2 `shouldSatisfy` null

  describe "Advanced graph tests" $ do
    it "should detect a cycle in a graph" $ do
      G.isCyclic cyclicGraph1 `shouldBe` True

    it "should detect acyclic graph" $ do
      G.isCyclic acyclicGraph1 `shouldBe` False

  describe "Minimum Spanning Tree tests" $ do
    it "should generate a non-self-looped MST" $ do
      let sptree = G.spanTree hexagon
        in G.isCyclic sptree `shouldBe` False
        
    it "should generate an MST which contains all nodes" $ do
      let { sptree = G.spanTree hexagon 
          ; ns     = G.nodes sptree 
          ; ns'    = map G.key ns
          }
        in ns' `shouldBe` ["A","B","C","D","E","F"]

    it "should generate an MST which contains all minimal edges" $ do
      let { sptree = G.spanTree hexagon
          ; eds    =  [ ("A","B") -- Make sure our spanning tree
                      , ("B","C") -- has all these edges.
                      , ("C","D")
                      ]
          }
        in all (\(n0, n1) -> G.hasLink n0 n1 sptree ) eds `shouldBe` True

