{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Qtility
import Types
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_qdb

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_qdb.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
