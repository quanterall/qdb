{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module TestUtilities where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import FileIO (FileOutput (..))
import Migration.Class (ReadMigrations (..), WriteMigrations (..))
import Qtility
import Qtility.Database (HasPostgresqlPool (..))
import Qtility.Database.Types
  ( Migration (..),
    MigrationNotFound (..),
    migrationFilename,
    migrationIsApplied,
  )
import Qtility.Time.Class (CurrentTime (..))
import qualified RIO.Map as Map
import RIO.Time (UTCTime)
import qualified RIO.Vector as Vector
import System.Console.ANSI (SGR)
import Terminal (TerminalOutput (..))
import Types (MigrationsPath (..))

data TestState = TestState
  { _testStateOutputLines :: IORef (Vector String),
    _testStateStyling :: IORef [SGR],
    _testStateIsVerbose :: IORef Bool,
    _testStateSqlPool :: ~(Pool Connection),
    _testStateFiles :: IORef (Map FilePath Text),
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

instance FileOutput (RIO TestState) where
  createDirectoryM _ = pure ()

  writeFileM path content = do
    files <- view testStateFiles
    modifyIORef' files $ Map.insert path content

instance CurrentTime (RIO TestState) where
  getCurrentTimeM = view testStateCurrentTime >>= readIORef

instance WriteMigrations (RIO TestState) where
  removeMigrationM path = do
    migrations <- view testStateMigrations
    modifyIORef' migrations (Map.delete path)

  -- createMigrationTableM path = do
  --   migrationsInDirectory <- view testStateMigrationsInDirectory >>= readIORef
  --   pure $ Map.findWithDefault [] path migrationsInDirectory

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

  migrationsInDirectoryM path = do
    fileMap <- view testStateMigrationsInDirectory >>= readIORef
    pure $ Map.findWithDefault [] path fileMap
