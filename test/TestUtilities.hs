{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module TestUtilities where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Qtility
import Qtility.Database (HasPostgresqlPool (..))
import qualified RIO.Vector as Vector
import System.Console.ANSI (SGR)
import Terminal (TerminalOut (..))

data TestState = TestState
  { _testStateOutputLines :: IORef (Vector String),
    _testStateStyling :: IORef [SGR],
    _testStateSqlPool :: Pool Connection,
    _testStateFiles :: Map FilePath ByteString
  }
  deriving (Generic)

foldMapM makeLenses [''TestState]

instance HasPostgresqlPool TestState where
  postgresqlPoolL = testStateSqlPool

instance TerminalOut (RIO TestState) where
  putStrLnM line = do
    linesRef <- view testStateOutputLines
    modifyIORef' linesRef (`Vector.snoc` line)
  setStylingM styling = do
    stylingRef <- view testStateStyling
    writeIORef stylingRef styling
