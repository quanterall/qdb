module Terminal
  ( TerminalOut (..),
    resetStyling,
    outputWithStyle,
  )
where

import Qtility
import System.Console.ANSI (SGR (..), setSGR)
import System.IO (putStrLn)
import Types

outputWithStyle :: (TerminalOut m) => [SGR] -> String -> m ()
outputWithStyle styling text = do
  setStylingM styling
  putStrLnM text
  resetStyling

resetStyling :: (TerminalOut m) => m ()
resetStyling = setStylingM [Reset]

class (Monad m) => TerminalOut m where
  putStrLnM :: String -> m ()
  setStylingM :: [SGR] -> m ()

instance TerminalOut IO where
  putStrLnM = putStrLn
  setStylingM = setSGR

instance TerminalOut (RIO App) where
  putStrLnM = putStrLn >>> liftIO
  setStylingM = setSGR >>> liftIO
