{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (ConnectInfo, Connection)
import Network.AWS.QAWS.SecretsManager.Types (SecretARN)
import Qtility
import Qtility.Database (HasPostgresqlPool (..))
import RIO.Process

newtype MigrationsPath = MigrationsPath {unMigrationsPath :: FilePath}
  deriving (Eq, Show)

data ConnectionInfo
  = RDSConnection !SecretARN
  | ManualConnection !ConnectInfo
  deriving (Eq, Show)

data AppCommand
  = Migrate !MigrationsPath !ConnectionInfo
  | Rollback !Int !ConnectionInfo
  | AddMigration !String !MigrationsPath
  | UpdateMigrations !MigrationsPath !ConnectionInfo
  | ListMigrations !ConnectionInfo
  | RemoveMigration !FilePath !ConnectionInfo
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
