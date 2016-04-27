module Networkhs.Graph where

import Prelude
import Data.Maybe
import Data.String
import Data.Map as M

data Node a = Node
	{
	  key      :: String
	, content  :: a
	}
	deriving (Show)

data Graph a = Graph
	{
	  nodes     :: [Node a]
	, linksTo   :: M.Map String [(String,Double)]
	, linksFrom :: M.Map String [(String,Double)]
	-- NOTE: Links are stored as redundancy in both directions
	-- just in order to speed up look up time in both ways.
	}
	deriving (Show)

data Route = Route [(String,String)]


----------------------------

newGraph :: [Node a] -> Graph a
newGraph ns = Graph { 
	nodes = ns, linksFrom = M.fromList [], linksTo = M.fromList []
	}

addNode :: (String,a) -> Graph a -> Graph a
addNode (s,a) g = g { nodes = (nodes g) ++ [Node s a]}

addLink :: Graph a -> (String,String,Double) -> Graph a
addLink g (n0,n1,w) = let g' = __addLinkTo g (n0,n1,w)
	in __addLinkFrom g' (n0,n1,w)

__addLinkTo ::  Graph a -> (String,String,Double) -> Graph a
__addLinkTo g (n0,n1,w) = let lnks = linksTo g in
	case M.lookup n1 lnks of
		Nothing -> g { linksTo = M.insert n1 [(n0,w)] lnks }
		Just array -> g { linksTo = M.insert n1 array'' lnks }
			where {
				array'  = [(a,w) | (a,w) <- array, a /= n0];
				array'' = array' ++ [(n0,w)]
			}

__addLinkFrom :: Graph a -> (String,String,Double) -> Graph a
__addLinkFrom g (n0,n1,w) = let lnks = linksFrom g in
	case M.lookup n0 lnks of
		Nothing -> g { linksFrom = M.insert n0 [(n1,w)] lnks }
		Just array -> g { linksFrom = M.insert n0 array'' lnks }
			where{
				array'  = [(b,w) | (b,w) <- array, b /= n1];
				array'' = array' ++ [(n1,w)]
			}
	

addBiLink :: Graph a -> (String,String,Double) -> Graph a
addBiLink g (n0,n1,w) = let g' = addLink g (n0,n1,w)
	in addLink g' (n1,n0,w)

link :: String -> String -> Graph a -> Maybe Double
link n0 n1 g = case M.lookup n0 (linksFrom g) of
	Nothing -> Nothing
	Just array -> 
		let ns = [ w | (n,w) <- array, n==n1]
		in if Prelude.null ns then Nothing else Just $ head ns

linkBackAndForth :: String -> String -> Graph a -> (Maybe Double, Maybe Double)
linkBackAndForth n0 n1 g = (link n0 n1 g, link n1 n0 g)

newRoute :: [String] -> Route
newRoute ns   = 
	-- Route must consist of at least two blocks of node
	if length ns < 2 then Route []
	else
		let { 
			n0 = head ns; 
			n1 = (head . tail) ns;
			r0 = [(n0,n1)];
			rn = newRoute $ tail ns;
		}
		in case rn of 
			Route [] -> Route r0
			Route rn' -> Route $ r0 ++ rn'

routeDistance :: Route -> Graph a -> Maybe Double
routeDistance r g = case r of
	Route [] -> Nothing
	Route rs -> let {
		(n0,n1) = head rs; -- First hop
		dist0   = link n0 n1 g; -- Distance of the first hop
		r'      = Route $ tail rs; -- The rest route succeeding the first hop
		dist'   = routeDistance r' g; -- Total distance of the rest
	}
		in case dist' of 
			Nothing -> Nothing -- Invalid succeeding route
			Just dist'' -> 
				case dist0 of
					Nothing -> Nothing -- Invalid first hop
					Just dist0' -> Just (dist0' + dist'')


