
module Main where

import Prelude hiding (FilePath)

import Control.Concurrent
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit

import Query
import View

loop
  :: (T.Text -> IO ())
  -> Maybe ThreadId
  -> Chan (Either T.Text (Maybe T.Text))
  -> View
  -> IO ()
loop output curQuery chan view = do
  ret <- readChan chan
  maybe (return ()) killThread curQuery
  case ret of
    Left query -> do
      clearResults view
      curQuery <- forkIO $ evalQuery (parseQuery query) (addResult view)
      loop output (Just curQuery) chan view
    Right mr -> do
      waitForView view
      case mr of
        Nothing -> exitFailure
        Just result -> output result >> exitSuccess

getOutput :: Maybe String -> T.Text -> IO ()
getOutput = maybe TIO.putStrLn (\fp -> TIO.writeFile fp . (`T.append` "\n"))

main :: IO ()
main = do
  args <- getArgs
  chan <- newChan
  view <- startView $ ViewOptions (writeChan chan . Left) (writeChan chan . Right)
  loop (getOutput . listToMaybe $ args) Nothing chan view

