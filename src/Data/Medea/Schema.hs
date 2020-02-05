module Data.Medea.Schema where

import Algebra.Graph.Acyclic.AdjacencyMap (AdjacencyMap)

import Data.Medea.Analysis (TypeNode)

-- placeholder
newtype Schema = Schema {
  typeGraph :: AdjacencyMap TypeNode
}
  deriving (Show)
