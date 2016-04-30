module Networkhs.Graph where

import Prelude
import Data.Maybe
import Data.String
import Data.List(sortBy)
import Data.Map as M hiding (map,foldl,filter)

data Node a = Node
  { key      :: String
  , content  :: a
  }
  deriving (Show)

data Graph a = Graph
  { nodes     :: [Node a]
  , linksTo   :: M.Map String [(String,Double)]
  , linksFrom :: M.Map String [(String,Double)]
  -- NOTE: Links are stored as redundancy in both directions
  -- just in order to speed up look up time in both ways.
  }
  deriving (Show)

data Route = Route [(String,String)]


----------------------------

-- | Create a new graph from given nodes
newGraph :: [Node a] -> Graph a
newGraph ns = Graph { nodes = ns
                    , linksFrom = M.fromList []
                    , linksTo = M.fromList []
                    }

-- | Add a new node to the graph
addNode :: (String,a) -> Graph a -> Graph a
addNode (s,a) g = g { nodes = (nodes g) ++ [Node s a]}

-- | Add a new link to the graph (idempotent function)
-- or replace an existing link with new weight
addLink :: Graph a -> (String,String,Double) -> Graph a
addLink g (n0,n1,w) = let g' = __addLinkTo g (n0,n1,w)
  in __addLinkFrom g' (n0,n1,w)

-- | Add a new directional link to a node (augmented function)
__addLinkTo ::  Graph a -> (String,String,Double) -> Graph a
__addLinkTo g (n0,n1,w) = let lnks = linksTo g in
  case M.lookup n1 lnks of
    Nothing -> g { linksTo = M.insert n1 [(n0,w)] lnks }
    Just array -> g { linksTo = M.insert n1 array'' lnks }
      where { array'  = [(a,w) | (a,w) <- array, a /= n0]
            ; array'' = array' ++ [(n0,w)]
            }

-- | Add a new directional link from a node to another node
-- (augmented function)
__addLinkFrom :: Graph a -> (String,String,Double) -> Graph a
__addLinkFrom g (n0,n1,w) = let lnks = linksFrom g in
  case M.lookup n0 lnks of
    Nothing -> g { linksFrom = M.insert n0 [(n1,w)] lnks }
    Just array -> g { linksFrom = M.insert n0 array'' lnks }
      where { array'  = [(b,w) | (b,w) <- array, b /= n1]
            ; array'' = array' ++ [(n1,w)]
            }

-- | Add a bidirectional link with the same weight
addBiLink :: Graph a -> (String,String,Double) -> Graph a
addBiLink g (n0,n1,w) = let g' = addLink g (n0,n1,w)
  in addLink g' (n1,n0,w)

-- | Get the weight of the link from a node to one another
link :: String -> String -> Graph a -> Maybe Double
link n0 n1 g = case M.lookup n0 (linksFrom g) of
  Nothing -> Nothing
  Just array -> 
    let ns = [ w | (n,w) <- array, n==n1]
    in if Prelude.null ns then Nothing else Just $ head ns

-- | Get links in both ways from a node to one another
linkBackAndForth :: String -> String -> Graph a -> (Maybe Double, Maybe Double)
linkBackAndForth n0 n1 g = (link n0 n1 g, link n1 n0 g)

-- | Comparison function for links
__orderLink :: (String,String,Double) -> (String,String,Double) -> Ordering
__orderLink (_,_,w1) (_,_,w2) = compare w1 w2

-- | Create a node route from given path
-- a path could be a list of node names in sequence
newRoute :: [String] -> Route
newRoute ns   = 
  -- Route must consist of at least two blocks of node
  if length ns < 2 then Route []
  else
    let { n0 = head ns
        ; n1 = (head . tail) ns
        ; r0 = [(n0,n1)]
        ; rn = newRoute $ tail ns
    }
    in case rn of 
      Route [] -> Route r0
      Route rn' -> Route $ r0 ++ rn'

-- | Calculate total distance to walk a route
routeDistance :: Route -> Graph a -> Maybe Double
routeDistance r g = case r of
  Route []     -> Nothing
  Route (q:[]) -> case q of
    (n0,n1)    -> link n0 n1 g 
    otherwise  -> Nothing -- Safeguard
  Route (q:qs) -> let { 
    dist0 = routeDistance (Route [q]) g;
    dists = routeDistance (Route qs) g;
    }
    in case dist0 of
      Nothing     -> Nothing
      Just dist0' -> case dists of
        Nothing     -> Nothing
        Just dists' -> Just (dist0' + dists')

-- | Expand a tuple of link (converted from map)
--   to a list of link tuples
__flattenLink :: (String,[(String,Double)]) -> [(String,String,Double)]
__flattenLink (n0,ts) = map (\(n1,w) -> (n0,n1,w) ) ts
__flattenLink otherwise = error "Invalid link pattern"

-- | Convert a link map to a plain list
__linksToList :: M.Map String [(String,Double)] -> [(String,String,Double)]
__linksToList m = let { ls  = M.toList m 
                      ; ls' = map __flattenLink ls
                      }
                  in foldl (++) [] ls'

-- | Get a list of edges of a graph, ordered by weight (smallest first)
edges :: Graph a -> [(String,String,Double)]
edges g = let { lnks  = linksFrom g 
              ; lls   = __linksToList lnks -- Expand the list of link tuples
  }
  in sortBy __orderLink lls

-- | Verify if a link is bidirectional (has its mirror)
__isBiLink :: Graph a -> (String,String,Double) -> Bool
__isBiLink g (n0,n1,w) = let (w1,w2) = linkBackAndForth n0 n1 g
  in w1 == w2

-- | Verify if a graph is undirected (all links are bi-directional)
isUndirected :: Graph a -> Bool
isUndirected g = let lnks = edges g
  in all (__isBiLink g) lnks

-- | Check whether a graph is cyclic
isCyclic :: Graph a -> Bool
isCyclic g = error "TAOTODO: Implement this"

-- | Check whether a link is a self loop
isSelfLoop :: (String,String,Double) -> Bool
isSelfLoop (n0,n1,w) = n0 == n1

-- | Accumulate a spanning tree with an edge (part of spanTree function)
__makeSpanTree :: Graph g -> (String,String,Double) -> Graph g
__makeSpanTree g l
  | isCyclic g' = g
  | otherwise = g'
    where g' = addLink g l

-- | Compute a minimum spanning tree of a graph (Kruskal's algorithm)
-- RESTRICTED: works only with digraph
spanTree :: Graph a -> Graph a
spanTree g 
  | isUndirected g = let{ es  = edges g
                        ; es' = filter (\e -> not . isSelfLoop $ e) es
                      }
                  in foldl __makeSpanTree (newGraph $ nodes g) es
  | otherwise = error "Spanning tree won't work with directed graph"



