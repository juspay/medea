module TestM.Util
  ( listMedeaFiles,
  )
where

import Data.List (sort)
import System.Directory (listDirectory)
import System.FilePath ((</>), isExtensionOf)

listMedeaFiles :: FilePath -> IO [FilePath]
listMedeaFiles dir = fmap (dir </>) . sort . filter (isExtensionOf ".medea") <$> listDirectory dir
