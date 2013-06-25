
{-# LANGUAGE BangPatterns #-}
module View where

import Prelude hiding (FilePath)

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import qualified Data.Map as M
import qualified Data.MeldableHeap as PQ
import qualified Data.Text as T
import Filesystem.Path
import Graphics.Vty

data ViewCommand =
    AddResult !Integer !T.Text
  | RemoveResult !T.Text
  | ClearResults
  | Shutdown
  deriving (Eq, Show)

newtype ViewHandle = ViewHandle { getChan :: Chan ViewCommand }

sendViewCommand :: (MonadIO m) => ViewCommand -> ViewHandle -> m ()
sendViewCommand !cmd hnd = liftIO $ writeChan (getChan hnd) cmd

data View =
  View
  { vOpts       :: ViewOptions
  , vResults    :: PQ.PQ (Integer, FilePath)
  , vResultsRev :: M.Map FilePath Integer
  , vChan       :: Chan ViewCommand
  , vScreen     :: Vty
  }

data ViewOptions =
  ViewOptions
  { voQueryHandler  :: T.Text -> IO ()
  , voResultHandler :: FilePath -> IO ()
  }

newView :: (MonadResource m) => ViewOptions -> ResourceT m ViewHandle
newView opts = do
  chan <- liftIO $ newChan
  view <- liftM (View opts PQ.empty M.empty chan . snd) $ allocate mkVty shutdown
  void $ liftM snd $ allocate (forkIO $ runView view) killThread
  return $ ViewHandle chan

type M a = ReaderT View IO a

runView :: View -> IO ()
runView = runReaderT $ render >> viewLoop

viewLoop :: M ()
viewLoop =
  asks vChan
  >>= liftIO . getChanContents
  >>= mapM_ runViewCommand . takeWhile (/= Shutdown)

runViewCommand :: ViewCommand -> M ()
runViewCommand (AddResult n xs) = addResult n xs
runViewCommand (RemoveResult xs) = removeResult xs
runViewCommand ClearResults = clearResults
runViewCommand Shutdown = error "runViewCommand Shutdown"

addResult :: Integer -> T.Text -> M ()
addResult = undefined

removeResult :: T.Text -> M ()
removeResult = undefined

clearResults :: M ()
clearResults = undefined

render :: M ()
render = undefined

