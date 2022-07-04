{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (ConnectInfo, Connection)
import Qtility
import Qtility.Database (HasPostgresqlPool (..))
import RIO.Process

newtype MigrationsPath = MigrationsPath {unMigrationsPath :: FilePath}
  deriving (Eq, Show)

data AppCommand
  = Migrate !MigrationsPath !ConnectInfo
  | Rollback !Int !ConnectInfo
  | AddMigration !String !MigrationsPath
  | UpdateMigrations !MigrationsPath !ConnectInfo
  | ListMigrations !ConnectInfo
  | RemoveMigration !FilePath !ConnectInfo
  deriving (Eq, Show)

newtype Options = Options {_optionsVerbose :: Bool}
  deriving (Eq, Show, Generic)

data App = App
  { _appLogFunc :: !LogFunc,
    _appProcessContext :: !ProcessContext,
    _appOptions :: !Options,
    _appSqlPool :: !(Pool Connection)
  }

foldMapM makeLenses [''Options, ''App]

foldMapM makeWrapped [''MigrationsPath]

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext

instance HasPostgresqlPool App where
  postgresqlPoolL = appSqlPool
