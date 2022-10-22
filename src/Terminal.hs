module Terminal
  ( Terminal (..),
    resetStyling,
    outputWithStyle,
  )
where

import Qtility
import System.Console.ANSI (SGR (..), setSGR)
import System.IO (putStrLn)
import Types

outputWithStyle :: (Terminal m) => [SGR] -> String -> m ()
outputWithStyle styling text = do
  setStylingM styling
  putStrLnM text
  resetStyling

resetStyling :: (Terminal m) => m ()
resetStyling = setStylingM [Reset]

class (Monad m) => Terminal m where
  putStrLnM :: String -> m ()
  setStylingM :: [SGR] -> m ()

instance Terminal IO where
  putStrLnM = putStrLn
  setStylingM = setSGR

instance Terminal (RIO App) where
  putStrLnM = putStrLn >>> liftIO
  setStylingM = setSGR >>> liftIO
