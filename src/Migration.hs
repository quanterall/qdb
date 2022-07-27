module Migration where

import Qtility
import Qtility.Database (DB, HasPostgresqlPool (..), runDB)
import Qtility.Database.Migration
import qualified Qtility.Database.Migration as Migration
import Qtility.Database.Migration.Queries
import Qtility.Database.Types
import RIO.FilePath ((</>))
import qualified RIO.Text as Text
import RIO.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Types

migrateAll ::
  (MonadReader env m, MonadThrow m, MonadIO m, HasPostgresqlPool env, HasLogFunc env) =>
  MigrationsPath ->
  m ()
migrateAll migrationsPath = do
  _ <- createMigrationTable schemaName $ migrationsPath ^. unwrap
  updateMigrations migrationsPath
  runDB $ do
    unappliedMigrations <- getUnappliedMigrations schemaName
    applyMigrations schemaName unappliedMigrations

rollback :: (MonadReader env m, MonadIO m, HasPostgresqlPool env) => Int -> m ()
rollback n = runDB $ rollbackLastNMigrations schemaName (fromIntegral n)

addMigration :: (MonadIO m) => String -> MigrationsPath -> m ()
addMigration name (MigrationsPath migrationsPath) = do
  timestamp <- getCurrentTimeInFormat
  let filename = timestamp <> "_-_" <> name <> ".sql"
  liftIO $ writeFileUtf8 (migrationsPath </> filename) migrationTemplate

updateMigrations ::
  (MonadReader env m, MonadIO m, MonadThrow m, HasPostgresqlPool env, HasLogFunc env) =>
  MigrationsPath ->
  m ()
updateMigrations (MigrationsPath migrationsPath) = do
  migrations <- migrationsInDirectory migrationsPath
  logDebug $ "Migrations: " <> displayShow migrations
  migrationOperations <- forM migrations $ \migration ->
    runDB $
      handle (handleMigrationNotFound migration) $ do
        updateMigration schemaName migration
        pure $ UpdatedMigration migration
  forM_ migrationOperations $ \case
    InsertedMigration migration ->
      logDebug $ "Inserted migration: " <> displayShow migration
    UpdatedMigration migration ->
      logDebug $ "Updated migration: " <> displayShow migration
  where
    handleMigrationNotFound :: Migration -> MigrationNotFound -> DB MigrationOperation
    handleMigrationNotFound migration _ = do
      insertMigrations schemaName [migration]
      pure $ InsertedMigration migration

listMigrations ::
  (MonadReader env m, MonadIO m, HasPostgresqlPool env, HasLogFunc env) =>
  Bool ->
  m ()
listMigrations verbose = do
  migrations <- runDB $ getMigrations schemaName
  forM_ migrations $ \migration -> do
    let outputString = [nameAndStatus] <> extraOutput & Text.intercalate "\n\n" & display
        extraOutput =
          if verbose
            then
              [ "Up:",
                migration ^. migrationUpStatement
                  & Text.lines
                  & fmap ("  " <>)
                  & Text.unlines,
                "Down:",
                migration
                  ^. migrationDownStatement
                  & Text.lines
                  & fmap ("  " <>)
                  & Text.unlines
              ]
            else []
        nameAndStatus =
          mconcat
            [ Text.pack $ migration ^. migrationFilename,
              " | ",
              if migration ^. migrationIsApplied then "Applied" else "Not applied"
            ]
    logInfo outputString

removeMigration' :: (MonadReader env m, MonadIO m, HasPostgresqlPool env) => FilePath -> m ()
removeMigration' filename = do
  runDB $ removeMigration schemaName filename

migrationTemplate :: Text
migrationTemplate =
  Text.unlines ["SELECT 1 + 1;", "", "-- DOWN", "", "SELECT 1 + 1;"]

getCurrentTimeInFormat :: (MonadIO m) => m String
getCurrentTimeInFormat = formatTime defaultTimeLocale Migration.timeFormat <$> getCurrentTime

schemaName :: Maybe DatabaseSchema
schemaName = Just "qdb"
