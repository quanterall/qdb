module FileIO where

import Qtility
import RIO.Directory (createDirectoryIfMissing)
import Types (App)

class (Monad m) => FileInput m where
  readFileM :: FilePath -> m Text

class (Monad m) => FileOutput m where
  writeFileM :: FilePath -> Text -> m ()
  createDirectoryM :: FilePath -> m ()

instance FileInput IO where
  readFileM = readFileUtf8

instance FileOutput IO where
  writeFileM = writeFileUtf8
  createDirectoryM = createDirectoryIfMissing True

instance FileInput (RIO App) where
  readFileM = readFileUtf8

instance FileOutput (RIO App) where
  writeFileM = writeFileUtf8
  createDirectoryM = createDirectoryIfMissing True
