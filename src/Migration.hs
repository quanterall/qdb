module Migration where

import Migration.Class (ApplyMigrations (..), ReadMigrations (..), WriteMigrations (..))
import Qtility
import Qtility.Database.Migration (migrationsInDirectory)
import qualified Qtility.Database.Migration as Migration
import Qtility.Database.Types
import Qtility.FileSystem (ReadFileSystem, WriteFileSystem (..))
import Qtility.Time.Class (CurrentTime (..))
import RIO.FilePath ((</>))
import qualified RIO.Text as Text
import RIO.Time (defaultTimeLocale, formatTime)
import qualified System.Console.ANSI.Codes as Codes
import Terminal (TerminalOutput (..), debugOutput, outputWithStyle, resetStyling)
import Types

migrateAll ::
  ( MonadThrow m,
    MonadUnliftIO m,
    TerminalOutput m,
    WriteMigrations m,
    ReadMigrations m,
    ReadFileSystem m,
    ApplyMigrations m
  ) =>
  MigrationsPath ->
  m ()
migrateAll migrationsPath = do
  _ <- createMigrationTableM migrationsPath
  updateMigrations migrationsPath
  unappliedMigrations <- getUnappliedMigrationsM
  applyMigrationsM unappliedMigrations

rollback :: (ApplyMigrations m) => Int -> m ()
rollback = rollbackMigrationsM

addMigration ::
  (TerminalOutput m, WriteFileSystem m, CurrentTime m) =>
  String ->
  MigrationsPath ->
  m ()
addMigration name (MigrationsPath migrationsPath) = do
  makeDirectoryM True migrationsPath
  timestamp <- getCurrentTimeInFormat
  let filename = timestamp <> "_-_" <> name <> ".sql"
  writeFileM (migrationsPath </> filename) migrationTemplate
  outputWithStyle [Codes.SetColor Codes.Foreground Codes.Vivid Codes.Green] $
    "Created migration '" <> filename <> "'"

updateMigrations ::
  forall m.
  ( MonadUnliftIO m,
    TerminalOutput m,
    WriteMigrations m,
    ReadFileSystem m,
    MonadThrow m
  ) =>
  MigrationsPath ->
  m ()
updateMigrations path = do
  createMigrationTableM path
  migrations <- migrationsInDirectory $ path ^. unwrap
  debugOutput $ "Migrations: " <> show migrations
  migrationOperations <- forM migrations $ \migration ->
    handle (handleMigrationNotFound migration) $ do
      oldMigration <- updateMigrationM migration
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
    handleMigrationNotFound :: Migration -> MigrationNotFound -> m MigrationOperation
    handleMigrationNotFound migration _ = do
      insertMigrationM migration
      pure $ InsertedMigration migration
    equalUpDown oldMigration migration =
      (oldMigration ^. migrationUpStatement, oldMigration ^. migrationDownStatement)
        == (migration ^. migrationUpStatement, migration ^. migrationDownStatement)

listMigrations :: (TerminalOutput m, ReadMigrations m) => Bool -> m ()
listMigrations verbose = do
  migrations <- getMigrationsM
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

removeMigration' :: (WriteMigrations m) => FilePath -> m ()
removeMigration' = removeMigrationM

migrationTemplate :: Text
migrationTemplate =
  Text.unlines ["SELECT 1 + 1;", "", "-- DOWN", "", "SELECT 1 + 1;"]

getCurrentTimeInFormat :: (CurrentTime m) => m String
getCurrentTimeInFormat = formatTime defaultTimeLocale Migration.timeFormat <$> getCurrentTimeM

schemaName :: Maybe DatabaseSchema
schemaName = Just "qdb"
