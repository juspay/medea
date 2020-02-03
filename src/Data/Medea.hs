module Data.Medea (
  loadSchema
) where

import Control.Monad (void)
import Data.Medea.Parsing (ParseError, parse)

-- TODO: This should return a schema graph when done
loadSchema :: FilePath -> IO (Either ParseError ())
loadSchema fp = void <$> parse fp
