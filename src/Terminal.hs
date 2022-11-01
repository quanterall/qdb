module Terminal
  ( TerminalOutput (..),
    resetStyling,
    outputWithStyle,
    debugOutput,
  )
where

import Qtility
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)
import System.IO (putStrLn)
import Types

class (Monad m) => TerminalOutput m where
  putStrLnM :: String -> m ()
  setStylingM :: [SGR] -> m ()
  isVerboseM :: m Bool

outputWithStyle :: (TerminalOutput m) => [SGR] -> String -> m ()
outputWithStyle styling text = do
  setStylingM styling
  putStrLnM text
  resetStyling

debugOutput :: (TerminalOutput m) => String -> m ()
debugOutput text = do
  whenM isVerboseM $ outputWithStyle [SetColor Foreground Vivid Cyan] text

resetStyling :: (TerminalOutput m) => m ()
resetStyling = setStylingM [Reset]

instance TerminalOutput IO where
  putStrLnM = putStrLn
  setStylingM = setSGR
  isVerboseM = pure False

instance TerminalOutput (RIO App) where
  putStrLnM = putStrLn >>> liftIO
  setStylingM = setSGR >>> liftIO
  isVerboseM = view $ appOptions . optionsVerbose
