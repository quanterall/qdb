{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module TestUtilities where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import FileIO (FileOutput (..))
import Migration.Class (ReadMigrations (..))
import Qtility
import Qtility.Database (HasPostgresqlPool (..))
import Qtility.Database.Types (Migration, migrationIsApplied)
import Qtility.Time.Class (CurrentTime (..))
import qualified RIO.Map as Map
import RIO.Time (UTCTime)
import qualified RIO.Vector as Vector
import System.Console.ANSI (SGR)
import Terminal (TerminalOutput (..))

data TestState = TestState
  { _testStateOutputLines :: IORef (Vector String),
    _testStateStyling :: IORef [SGR],
    _testStateIsVerbose :: IORef Bool,
    _testStateSqlPool :: ~(Pool Connection),
    _testStateFiles :: IORef (Map FilePath ByteString),
    _testStateCurrentTime :: IORef UTCTime,
    _testStateMigrations :: IORef [Migration]
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
  writeFileM path content = do
    files <- view testStateFiles
    modifyIORef' files (content & encodeUtf8 & Map.insert path)

  createDirectoryM _ = pure ()

instance CurrentTime (RIO TestState) where
  getCurrentTimeM = view testStateCurrentTime >>= readIORef

instance ReadMigrations (RIO TestState) where
  getMigrationsM = view testStateMigrations >>= readIORef
  getUnappliedMigrationsM = filter ((^. migrationIsApplied) >>> not) <$> getMigrationsM
