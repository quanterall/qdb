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
import qualified System.Console.ANSI.Codes as Codes
import Terminal (TerminalOut (..), outputWithStyle, resetStyling)
import Types

migrateAll ::
  (MonadReader env m, MonadThrow m, MonadIO m, TerminalOut m, HasPostgresqlPool env, HasLogFunc env) =>
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

addMigration :: (MonadIO m, TerminalOut m) => String -> MigrationsPath -> m ()
addMigration name (MigrationsPath migrationsPath) = do
  timestamp <- getCurrentTimeInFormat
  let filename = timestamp <> "_-_" <> name <> ".sql"
  liftIO $ writeFileUtf8 (migrationsPath </> filename) migrationTemplate
  outputWithStyle [Codes.SetColor Codes.Foreground Codes.Vivid Codes.Green] $
    "Created migration '" <> filename <> "'"

updateMigrations ::
  ( MonadReader env m,
    MonadIO m,
    MonadThrow m,
    TerminalOut m,
    HasPostgresqlPool env,
    HasLogFunc env
  ) =>
  MigrationsPath ->
  m ()
updateMigrations (MigrationsPath migrationsPath) = do
  createMigrationTable schemaName migrationsPath
  migrations <- migrationsInDirectory migrationsPath
  logDebug $ "Migrations: " <> displayShow migrations
  migrationOperations <- forM migrations $ \migration ->
    runDB $
      handle (handleMigrationNotFound migration) $ do
        oldMigration <- updateMigration schemaName migration
        pure $
          if oldMigration `equalUpDown` migration
            then UnchangedMigration migration
            else UpdatedMigration migration
  forM_ migrationOperations $ \case
    InsertedMigration migration -> do
      outputWithStyle [Codes.SetColor Codes.Foreground Codes.Vivid Codes.Green] $
        "Inserted migration: " <> migration ^. migrationFilename
    UpdatedMigration migration -> do
      outputWithStyle [Codes.SetColor Codes.Foreground Codes.Vivid Codes.Yellow] $
        "Updated migration: " <> migration ^. migrationFilename
    UnchangedMigration _migration -> pure ()
  where
    handleMigrationNotFound :: Migration -> MigrationNotFound -> DB MigrationOperation
    handleMigrationNotFound migration _ = do
      insertMigrations schemaName [migration]
      pure $ InsertedMigration migration
    equalUpDown oldMigration migration =
      (oldMigration ^. migrationUpStatement, oldMigration ^. migrationDownStatement)
        == (migration ^. migrationUpStatement, migration ^. migrationDownStatement)

listMigrations ::
  (MonadReader env m, MonadIO m, TerminalOut m, HasPostgresqlPool env) =>
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
    setStylingM [Codes.SetColor Codes.Foreground Codes.Vivid foregroundColor]
    putStrLnM outputString
    resetStyling

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
