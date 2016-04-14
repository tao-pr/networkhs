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
	  nodes    :: [Node a]
	, links    :: M.Map String Double
	}
	deriving (Show)


----------------------------

newGraph :: [Node a] -> Graph a
newGraph ns = Graph { nodes = ns, links = M.fromList []}

linkKey :: String -> String -> String
linkKey a b = unlines [a,b]

addLink :: (String,String,Double) -> Graph a -> Graph a
addLink (n0,n1,w) g = let lmap = links g 
	in g { links = M.insert (linkKey n0 n1) w lmap } -- This will always update existing link

