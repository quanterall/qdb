module Terminal
  ( TerminalOutput (..),
    resetStyling,
    outputWithStyle,
  )
where

import Qtility
import System.Console.ANSI (SGR (..), setSGR)
import System.IO (putStrLn)
import Types

outputWithStyle :: (TerminalOutput m) => [SGR] -> String -> m ()
outputWithStyle styling text = do
  setStylingM styling
  putStrLnM text
  resetStyling

resetStyling :: (TerminalOutput m) => m ()
resetStyling = setStylingM [Reset]

class (Monad m) => TerminalOutput m where
  putStrLnM :: String -> m ()
  setStylingM :: [SGR] -> m ()

instance TerminalOutput IO where
  putStrLnM = putStrLn
  setStylingM = setSGR

instance TerminalOutput (RIO App) where
  putStrLnM = putStrLn >>> liftIO
  setStylingM = setSGR >>> liftIO
