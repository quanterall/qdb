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
import System.Console.ANSI (setSGR)
import qualified System.Console.ANSI.Codes as Codes
import System.IO (putStrLn)
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
  createMigrationTable schemaName migrationsPath
  migrations <- migrationsInDirectory migrationsPath
  logDebug $ "Migrations: " <> displayShow migrations
  migrationOperations <- forM migrations $ \migration ->
    runDB $
      handle (handleMigrationNotFound migration) $ do
        updateMigration schemaName migration
        pure $ UpdatedMigration migration
  forM_ migrationOperations $ \case
    InsertedMigration migration -> do
      liftIO $ setSGR [Codes.SetColor Codes.Foreground Codes.Vivid Codes.Green]
      liftIO $ putStrLn $ "Inserted migration: " <> migration ^. migrationFilename
      liftIO $ setSGR [Codes.Reset]
    UpdatedMigration migration -> do
      liftIO $ setSGR [Codes.SetColor Codes.Foreground Codes.Vivid Codes.Yellow]
      liftIO $ putStrLn $ "Updated migration: " <> migration ^. migrationFilename
      liftIO $ setSGR [Codes.Reset]
  where
    handleMigrationNotFound :: Migration -> MigrationNotFound -> DB MigrationOperation
    handleMigrationNotFound migration _ = do
      insertMigrations schemaName [migration]
      pure $ InsertedMigration migration

listMigrations ::
  (MonadReader env m, MonadIO m, HasPostgresqlPool env) =>
  Bool ->
  m ()
listMigrations verbose = do
  migrations <- runDB $ getMigrations schemaName
  forM_ migrations $ \migration -> do
    let outputString = [nameAndStatus] <> extraOutput & Text.intercalate "\n\n" & Text.unpack
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
    let foregroundColor = migration ^. migrationIsApplied & bool Codes.Red Codes.Green
    liftIO $ setSGR [Codes.SetColor Codes.Foreground Codes.Vivid foregroundColor]
    liftIO $ putStrLn outputString
    liftIO $ setSGR [Codes.Reset]

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
