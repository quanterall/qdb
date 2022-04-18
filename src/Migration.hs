module Migration where

import Database.PostgreSQL.Simple.Migration
import qualified Database.PostgreSQL.Simple.Migration as Migration
import Database.PostgreSQL.Simple.Migration.Queries
import Database.PostgreSQL.Simple.Migration.Types
import Database.PostgreSQL.Simple.Utilities (DB, runDB)
import Qtility
import RIO.FilePath ((</>))
import qualified RIO.Text as Text
import RIO.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (putStrLn)
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

updateMigrations :: RIO App ()
updateMigrations = do
  migrationsPath <- view (appOptions . optionsMigrationsPath)
  migrations <- migrationsInDirectory migrationsPath
  forM_ migrations $ \migration ->
    runDB $ handle (handleMigrationNotFound migration) $ updateMigration Nothing migration
  where
    handleMigrationNotFound :: Migration -> MigrationNotFound -> DB ()
    handleMigrationNotFound migration _ = do
      insertMigrations Nothing [migration]

listMigrations :: Bool -> RIO App ()
listMigrations verbose = do
  migrations <- runDB $ getMigrations Nothing
  forM_ migrations $ \migration -> do
    let outputString =
          if verbose
            then
              unlines
                [ mconcat
                    [ migration ^. migrationFilename,
                      " | ",
                      if migration ^. migrationIsApplied then "Applied" else "Not applied"
                    ],
                  migration ^. migrationUpStatement
                    & Text.lines
                    & fmap ("  " <>)
                    & Text.unlines
                    & Text.unpack,
                  migration ^. migrationDownStatement
                    & Text.lines
                    & fmap ("  " <>)
                    & Text.unlines
                    & Text.unpack
                ]
            else
              mconcat
                [ migration ^. migrationFilename,
                  " | ",
                  if migration ^. migrationIsApplied then "Applied" else "Not applied"
                ]
    liftIO $ putStrLn outputString

removeMigration' :: FilePath -> RIO App ()
removeMigration' filename = do
  runDB $ removeMigration Nothing filename

migrationTemplate :: Text
migrationTemplate =
  Text.unlines ["SELECT 1 + 1;", "", "-- DOWN", "", "SELECT 1 + 1;"]

getCurrentTimeInFormat :: (MonadIO m) => m String
getCurrentTimeInFormat = formatTime defaultTimeLocale Migration.timeFormat <$> getCurrentTime
