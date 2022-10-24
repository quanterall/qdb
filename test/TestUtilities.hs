{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module TestUtilities where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import FileIO (FileOutput (..))
import Qtility
import Qtility.Database (HasPostgresqlPool (..))
import Qtility.Time.Class (CurrentTime (..))
import qualified RIO.Map as Map
import RIO.Time (UTCTime)
import qualified RIO.Vector as Vector
import System.Console.ANSI (SGR)
import Terminal (TerminalOutput (..))

data TestState = TestState
  { _testStateOutputLines :: IORef (Vector String),
    _testStateStyling :: IORef [SGR],
    _testStateSqlPool :: ~(Pool Connection),
    _testStateFiles :: IORef (Map FilePath ByteString),
    _testStateCurrentTime :: IORef UTCTime
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

instance FileOutput (RIO TestState) where
  writeFileM path content = do
    files <- view testStateFiles
    modifyIORef' files (content & encodeUtf8 & Map.insert path)

  createDirectoryM _ = pure ()

instance CurrentTime (RIO TestState) where
  getCurrentTimeM = view testStateCurrentTime >>= readIORef
