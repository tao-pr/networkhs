module Networkhs.Graph where

import Prelude
import Data.Maybe

data Node a = Node
	{
		key      :: String
	, content  :: a
	}

data Link = Link
	{
		from     :: String
	, to       :: String
	, weight   :: Double
	}

data Graph a = Graph
	{
		nodes    :: [Node a]
	, links    :: [Link]
	}

