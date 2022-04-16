module Run (run) where

import Qtility
import Types

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
