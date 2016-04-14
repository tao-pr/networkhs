module Networkhs.Graph where

import Prelude
import Data.Maybe

data Node a = Node
	{
		key      :: String
	, content  :: a
	}
	deriving (Show)

data Link = Link
	{
		from     :: String
	, to       :: String
	, weight   :: Double
	}
	deriving (Show)

data Graph a = Graph
	{
		nodes    :: [Node a]
	, links    :: [Link]
	}
	deriving (Show)


----------------------------

newGraph :: [Node a] -> Graph a
newGraph ns = Graph { nodes = ns, links = []}


