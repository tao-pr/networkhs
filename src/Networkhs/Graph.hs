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

addLink :: (String,String,Double) -> Graph a -> Graph a
addLink (n0,n1,w) g = let g' = __addLinkTo (n0,n1,w) g
	in __addLinkFrom (n0,n1,w) g'

__addLinkTo :: (String,String,Double) -> Graph a -> Graph a
__addLinkTo (n0,n1,w) g = let lnks = linksTo g in
	case M.lookup n1 lnks of
		Nothing -> g { linksTo = M.insert n1 [(n0,w)] lnks }
		Just array -> g { linksTo = M.insert n1 array'' lnks }
			where {
				array'  = [(a,w) | (a,w) <- array, a /= n0];
				array'' = array' ++ [(n0,w)]
			}

__addLinkFrom :: (String,String,Double) -> Graph a -> Graph a
__addLinkFrom (n0,n1,w) g = let lnks = linksFrom g in
	case M.lookup n0 lnks of
		Nothing -> g { linksFrom = M.insert n0 [(n1,w)] lnks }
		Just array -> g { linksFrom = M.insert n0 array'' lnks }
			where{
				array'  = [(b,w) | (b,w) <- array, b /= n1];
				array'' = array' ++ [(n1,w)]
			}
	

addBiLink :: (String,String,Double) -> Graph a -> Graph a
addBiLink (n0,n1,w) g = let g' = addLink (n0,n1,w) g
	in addLink (n1,n0,w) g'

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
		(n0,n1) = head rs;
		dist0   = link n0 n1 g; -- Distance of the first hop
		dist'   = routeDistance $ Route $ tail rs; -- Total distance of the rest
	}
	in if null dist0 || null dist' then Nothing else 

__distance :: (String,String) -> Graph a -> Maybe Double
__distance (n0,n1) g = link n0 n1 g


