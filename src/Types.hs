{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (ConnectInfo, Connection)
import Network.AWS.QAWS.SecretsManager.Types (SecretARN)
import Qtility
import Qtility.Database (HasPostgresqlPool (..))
import Qtility.Database.Types (Migration)
import Qtility.FileSystem (ReadFileSystem (..))
import RIO.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import RIO.Process

data ConfigurationFileOptions = ConfigurationFileOptions
  { _configurationFileOptionsMigrationsPath :: !(Maybe FilePath),
    _configurationFileOptionsSecretArn :: !(Maybe String),
    _configurationFileOptionsHost :: !(Maybe String),
    _configurationFileOptionsPort :: !(Maybe Word16),
    _configurationFileOptionsUser :: !(Maybe String),
    _configurationFileOptionsPassword :: !(Maybe String),
    _configurationFileOptionsDatabase :: !(Maybe String)
  }
  deriving (Eq, Show, Generic)

newtype MigrationsPath = MigrationsPath {unMigrationsPath :: FilePath}
  deriving (Eq, Show, Ord)

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

data MigrationOperation
  = InsertedMigration Migration
  | UpdatedMigration Migration
  | UnchangedMigration Migration
  deriving (Eq, Show)

data Options = Options {_optionsVerbose :: !Bool, _optionsConfigurationPath :: !FilePath}
  deriving (Eq, Show, Generic)

data App = App
  { _appLogFunc :: !LogFunc,
    _appProcessContext :: !ProcessContext,
    _appOptions :: !Options,
    _appSqlPool :: !(Pool Connection)
  }

foldMapM deriveLensAndJSON [''ConfigurationFileOptions]

foldMapM makeLenses [''Options, ''App]

foldMapM makeWrapped [''MigrationsPath]

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext

instance HasPostgresqlPool App where
  postgresqlPoolL = appSqlPool

instance ReadFileSystem (RIO App) where
  readFileM = readFileUtf8 >>> liftIO
  readByteStringFileM = readFileBinary >>> liftIO
  listDirectoryM = listDirectory >>> liftIO
  doesFileExistM = doesFileExist >>> liftIO
  doesDirectoryExistM = doesDirectoryExist >>> liftIO
