{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Qtility
import Qtility.Database (HasPostgresqlPool (..))
import RIO.Process

data AppCommand
  = Migrate
  | Rollback !Int
  | AddMigration !String
  | UpdateMigrations
  | ListMigrations
  | RemoveMigration !FilePath
  deriving (Eq, Show)

-- | Command line arguments
data Options = Options
  { _optionsVerbose :: !Bool,
    _optionsHost :: !String,
    _optionsPort :: !Word16,
    _optionsUser :: !String,
    _optionsDatabase :: !String,
    _optionsPassword :: !String,
    _optionsMigrationsPath :: !FilePath
  }
  deriving (Eq, Show, Generic)

data App = App
  { _appLogFunc :: !LogFunc,
    _appProcessContext :: !ProcessContext,
    _appOptions :: !Options,
    _appSqlPool :: !(Pool Connection)
  }

foldMapM makeLenses [''Options, ''App]

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext

instance HasPostgresqlPool App where
  postgresqlPoolL = appSqlPool
