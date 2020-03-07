module Data.Medea.Schema where

import Algebra.Graph.Acyclic.AdjacencyMap (AdjacencyMap)
import Data.Map.Strict (Map)

import Data.Medea.Analysis (TypeNode, ReducedSchema)
import Data.Medea.Parser.Primitive (Identifier)


-- placeholder
data Schema = Schema {
  typeGraph :: AdjacencyMap TypeNode,
  reducedSpec :: Map Identifier ReducedSchema
}
  deriving (Show)
