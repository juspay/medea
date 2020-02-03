module Data.Medea (
  loadSchema
) where

import Control.Monad (void)

import Data.Medea.Parsing (parse)
import Data.Medea.Error (LoaderError)
import Data.Medea.SchemaGraph (analyze)

-- TODO: This should return a schema graph when done
loadSchema :: FilePath -> IO (Either LoaderError ())
loadSchema fp = do
  parseStep <- parse fp
  let analysisStep = parseStep >>= analyze
  pure . void $ analysisStep
