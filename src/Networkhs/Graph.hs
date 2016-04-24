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


----------------------------

newGraph :: [Node a] -> Graph a
newGraph ns = Graph { 
	nodes = ns, linksFrom = M.fromList [], linksTo = M.fromList []
	}


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
