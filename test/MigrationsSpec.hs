module MigrationsSpec where

import Migration (addMigration, getCurrentTimeInFormat, listMigrations)
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
      isVerbose <- liftIO $ newIORef False
      files <- liftIO $ newIORef mempty
      currentTime <- liftIO getCurrentTime >>= newIORef
      migrationsRef <- liftIO $ newIORef mempty
      let testState =
            TestState
              { _testStateOutputLines = outputLines,
                _testStateStyling = styling,
                _testStateIsVerbose = isVerbose,
                _testStateFiles = files,
                _testStateCurrentTime = currentTime,
                _testStateSqlPool = undefined,
                _testStateMigrations = migrationsRef
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
