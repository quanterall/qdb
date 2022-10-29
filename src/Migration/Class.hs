module Migration.Class
  ( ReadMigrations (..),
    WriteMigrations (..),
    ApplyMigrations (..),
  )
where

import Qtility
import Qtility.Database (runDB)
import Qtility.Database.Migration (createMigrationTable)
import Qtility.Database.Migration.Queries
  ( applyMigrations,
    getMigrations,
    getUnappliedMigrations,
    insertMigrations,
    removeMigration,
    rollbackLastNMigrations,
    updateMigration,
  )
import Qtility.Database.Types (DatabaseSchema, Migration)
import Types (App, MigrationsPath (..))

class (Monad m) => ReadMigrations m where
  getMigrationsM :: m [Migration]
  getUnappliedMigrationsM :: m [Migration]

instance ReadMigrations (RIO App) where
  getMigrationsM = schemaName & getMigrations & runDB
  getUnappliedMigrationsM = schemaName & getUnappliedMigrations & runDB

class (Monad m) => WriteMigrations m where
  removeMigrationM :: FilePath -> m ()
  createMigrationTableM :: MigrationsPath -> m [Migration]
  updateMigrationM :: Migration -> m Migration
  insertMigrationM :: Migration -> m ()

instance WriteMigrations (RIO App) where
  removeMigrationM = removeMigration schemaName >>> runDB
  createMigrationTableM = (^. unwrap) >>> createMigrationTable schemaName
  updateMigrationM = updateMigration schemaName >>> runDB
  insertMigrationM = pure >>> insertMigrations schemaName >>> runDB

class (Monad m) => ApplyMigrations m where
  applyMigrationsM :: [Migration] -> m ()
  rollbackMigrationsM :: Int -> m ()

instance ApplyMigrations (RIO App) where
  applyMigrationsM = applyMigrations schemaName >>> runDB
  rollbackMigrationsM = fromIntegral >>> rollbackLastNMigrations schemaName >>> runDB

schemaName :: Maybe DatabaseSchema
schemaName = Just "qdb"
