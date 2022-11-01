{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module TestUtilities where

import Control.Lens (_1)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Migration.Class (ReadMigrations (..), WriteMigrations (..))
import Qtility
import Qtility.Database (HasPostgresqlPool (..))
import Qtility.Database.Types
  ( Migration (..),
    MigrationNotFound (..),
    migrationFilename,
    migrationIsApplied,
  )
import Qtility.FileSystem (ReadFileSystem (..), WriteFileSystem (..))
import Qtility.Time.Class (CurrentTime (..))
import RIO.FilePath (dropTrailingPathSeparator, splitFileName)
import qualified RIO.Map as Map
import RIO.Time (UTCTime)
import qualified RIO.Vector as Vector
import System.Console.ANSI (SGR)
import qualified System.IO.Error as IOError
import Terminal (TerminalOutput (..))
import Types (MigrationsPath (..))

data TestState = TestState
  { _testStateOutputLines :: IORef (Vector String),
    _testStateStyling :: IORef [SGR],
    _testStateIsVerbose :: IORef Bool,
    _testStateSqlPool :: ~(Pool Connection),
    _testStateFiles :: IORef (Map FilePath (Map FilePath Text)),
    _testStateCurrentTime :: IORef UTCTime,
    _testStateMigrations :: IORef (Map FilePath Migration)
  }
  deriving (Generic)

foldMapM makeLenses [''TestState]

instance HasPostgresqlPool TestState where
  postgresqlPoolL = testStateSqlPool

instance TerminalOutput (RIO TestState) where
  putStrLnM line = do
    linesRef <- view testStateOutputLines
    modifyIORef' linesRef (`Vector.snoc` line)

  setStylingM styling = do
    stylingRef <- view testStateStyling
    writeIORef stylingRef styling

  isVerboseM = view testStateIsVerbose >>= readIORef

instance CurrentTime (RIO TestState) where
  getCurrentTimeM = view testStateCurrentTime >>= readIORef

instance WriteMigrations (RIO TestState) where
  removeMigrationM path = do
    migrations <- view testStateMigrations
    modifyIORef' migrations (Map.delete path)

  updateMigrationM migration = do
    migrations <- view testStateMigrations
    hasMigration <- Map.member (migration ^. migrationFilename) <$> readIORef migrations
    if hasMigration
      then modifyIORef' migrations (Map.insert (migration ^. migrationFilename) migration)
      else throwM $ MigrationNotFound (migration ^. migrationFilename)
    pure migration

  insertMigrationM migration = do
    migrations <- view testStateMigrations
    modifyIORef' migrations $ Map.insert (migration ^. migrationFilename) migration

instance ReadMigrations (RIO TestState) where
  getMigrationsM = do
    migrations <- view testStateMigrations >>= readIORef
    pure $ Map.elems migrations

  getUnappliedMigrationsM = filter ((^. migrationIsApplied) >>> not) <$> getMigrationsM

instance ReadFileSystem (RIO TestState) where
  readFileM path = do
    let (directory, filename) = path & splitFileName & _1 %~ dropTrailingPathSeparator
    files <- view testStateFiles >>= readIORef
    case Map.lookup directory files >>= Map.lookup filename of
      Just contents -> pure contents
      Nothing ->
        throwM $ IOError.mkIOError IOError.doesNotExistErrorType "readFileM" Nothing (Just path)

  readByteStringFileM path = do
    let (dir, file) = splitFileName path
    files <- view testStateFiles >>= readIORef
    case Map.lookup dir files >>= Map.lookup file of
      Just contents -> pure $ encodeUtf8 contents
      Nothing ->
        throwM $
          IOError.mkIOError IOError.doesNotExistErrorType "readByteStringFileM" Nothing (Just path)

  listDirectoryM path = do
    files <- view testStateFiles >>= readIORef
    case Map.lookup path files of
      Just contents -> pure $ Map.keys contents
      Nothing ->
        throwM $
          IOError.mkIOError IOError.doesNotExistErrorType "listDirectoryM" Nothing (Just path)

  doesDirectoryExistM path = do
    files <- view testStateFiles >>= readIORef
    pure $ Map.member path files

  doesFileExistM path = do
    let (dir, file) = splitFileName path
    files <- view testStateFiles >>= readIORef
    case Map.lookup dir files of
      Just contents -> pure $ Map.member file contents
      Nothing -> pure False

instance WriteFileSystem (RIO TestState) where
  writeFileM path contents = do
    let (dir, file) = path & splitFileName & _1 %~ dropTrailingPathSeparator
    files <- view testStateFiles
    modifyIORef' files $ Map.alter (fromMaybe Map.empty >>> Map.insert file contents >>> Just) dir

  writeByteStringFileM path contents = do
    let (dir, file) = splitFileName path
    files <- view testStateFiles
    modifyIORef' files $
      Map.alter (fromMaybe Map.empty >>> Map.insert file (decodeUtf8Lenient contents) >>> Just) dir

  removeFileM path = do
    let (dir, file) = splitFileName path
    files <- view testStateFiles
    modifyIORef' files $ Map.alter (fromMaybe Map.empty >>> Map.delete file >>> Just) dir

  makeDirectoryM _createParents path = do
    files <- view testStateFiles
    modifyIORef' files $ Map.insertWith (<>) path Map.empty

  removeDirectoryM path = do
    files <- view testStateFiles
    modifyIORef' files $ Map.delete path
