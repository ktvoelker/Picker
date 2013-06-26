
{-# LANGUAGE BangPatterns #-}
module View where

import Prelude hiding (FilePath)

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Lens
import qualified Data.MeldableHeap as PQ
import Filesystem.Path.CurrentOS
import Graphics.Vty

import View.Types

sendViewCommand :: (MonadIO m) => ViewCommand -> ViewHandle -> m ()
sendViewCommand !cmd hnd = liftIO $ writeChan (getChan hnd) cmd

newView :: (MonadResource m) => ViewOptions -> ResourceT m ViewHandle
newView opts = do
  chan <- liftIO $ newChan
  view <- liftM (View opts PQ.empty chan . snd) $ allocate mkVty shutdown
  void $ liftM snd $ allocate (forkIO $ runView view) killThread
  return $ ViewHandle chan

runView :: View -> IO ()
runView = evalStateT $ render >> viewLoop

viewLoop :: M ()
viewLoop =
  access vChan
  >>= liftIO . getChanContents
  >>= mapM_ runViewCommand . takeWhile (/= Shutdown)

runViewCommand :: ViewCommand -> M ()
runViewCommand (AddResult n xs) = addResult n $ fromText xs
runViewCommand ClearResults = clearResults
runViewCommand Shutdown = error "runViewCommand Shutdown"

addResult :: Integer -> FilePath -> M ()
addResult n fp = do
  void $ vResults %= PQ.insert (n, fp)
  render

clearResults :: M ()
clearResults = do
  void $ vResults ~= PQ.empty
  render

render :: M ()
render = undefined

