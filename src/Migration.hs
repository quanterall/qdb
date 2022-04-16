module Migration where

import Database.PostgreSQL.Simple.Migration
import qualified Database.PostgreSQL.Simple.Migration as Migration
import Database.PostgreSQL.Simple.Migration.Queries
import Database.PostgreSQL.Simple.Utilities (runDB)
import Qtility
import RIO.FilePath ((</>))
import qualified RIO.Text as Text
import RIO.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Types

migrateAll :: RIO App ()
migrateAll = do
  migrationsPath <- view (appOptions . optionsMigrationsPath)
  _ <- createMigrationTable Nothing migrationsPath
  runDB $ do
    unappliedMigrations <- getUnappliedMigrations Nothing
    applyMigrations Nothing unappliedMigrations

rollback :: Int -> RIO App ()
rollback n = runDB $ rollbackLastNMigrations Nothing (fromIntegral n)

addMigration :: String -> RIO App ()
addMigration name = do
  migrationsPath <- view (appOptions . optionsMigrationsPath)
  timestamp <- getCurrentTimeInFormat
  let filename = timestamp <> "_-_" <> name <> ".sql"
  liftIO $ writeFileUtf8 (migrationsPath </> filename) migrationTemplate

migrationTemplate :: Text
migrationTemplate =
  Text.unlines ["SELECT 1 + 1;", "", "-- DOWN", "", "SELECT 1 + 1;"]

getCurrentTimeInFormat :: (MonadIO m) => m String
getCurrentTimeInFormat = formatTime defaultTimeLocale Migration.timeFormat <$> getCurrentTime
