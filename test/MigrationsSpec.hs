module MigrationsSpec where

import Migration (addMigration, getCurrentTimeInFormat)
import Qtility
import RIO.Time (getCurrentTime)
import qualified RIO.Vector as Vector
import Test.Hspec
import TestUtilities (TestState (..))
import Types (MigrationsPath (..))

spec :: Spec
spec = do
  describe "Migrations" $ do
    it "should output lines to the terminal when we execute migration functions" $ do
      outputLines <- liftIO $ newIORef mempty
      styling <- liftIO $ newIORef []
      files <- liftIO $ newIORef mempty
      currentTime <- liftIO getCurrentTime >>= newIORef
      let testState =
            TestState
              { _testStateOutputLines = outputLines,
                _testStateStyling = styling,
                _testStateFiles = files,
                _testStateCurrentTime = currentTime,
                _testStateSqlPool = undefined
              }
      currentTimeString <- runRIO testState getCurrentTimeInFormat
      runRIO testState $ do
        addMigration "migration-name" $ MigrationsPath "migrations"

      liftIO (readIORef outputLines)
        `shouldReturn` Vector.fromList
          [ mconcat
              [ "Created migration '",
                currentTimeString,
                "_-_migration-name.sql'"
              ]
          ]
