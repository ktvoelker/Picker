
module Main where

import Prelude hiding (FilePath)

import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit

import Query
import View

loop :: Maybe ThreadId -> Chan (Either T.Text (Maybe T.Text)) -> View -> IO ()
loop curQuery chan view = do
  ret <- readChan chan
  maybe (return ()) killThread curQuery
  case ret of
    Left query -> do
      clearResults view
      curQuery <- forkIO $ evalQuery (parseQuery query) (addResult view)
      loop (Just curQuery) chan view
    Right mr -> do
      waitForView view
      case mr of
        Nothing -> exitFailure
        Just result -> TIO.putStrLn result >> exitSuccess

main :: IO ()
main = do
  chan <- newChan
  view <- startView $ ViewOptions (writeChan chan . Left) (writeChan chan . Right)
  loop Nothing chan view

