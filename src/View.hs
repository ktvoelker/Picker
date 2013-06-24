
module View where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan.Strict
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.Map as M
import qualified Data.MeldableHeap as PQ
import Data.Text
import Filesystem.Path
import Graphics.Vty

data ViewCommand =
    AddResult !Integer !Text
  | RemoveResult !Text
  | ClearResults
  deriving (Eq, Show)

instance NFData ViewCommand

newtype ViewHandle = ViewHandle { getChan :: Chan ViewCommand }

sendViewCommand :: (MonadIO m) => ViewCommand -> ViewHandle -> m ()
sendViewCommand cmd hnd = liftIO $ writeChan (getChan hnd) cmd

data View =
  View
  { vOpts       :: ViewOptions
  , vResults    :: PQ.PQ (Integer, FilePath)
  , vResultsRev :: M.Map FilePath Integer
  , vScreen     :: Vty
  }

data ViewOptions =
  ViewOptions
  { voQueryHandler  :: Text -> IO ()
  , voResultHandler :: FilePath -> IO ()
  }

-- TODO return a ViewHandle instead, and make sure the ResourceT can clean up
-- both the thread and the vty stuff
newView :: (MonadResource m) => ViewOptions -> ResourceT m View
newView opts = liftM (View opts PQ.empty M.empty . snd) $ allocate mkVty shutdown

addResult :: (MonadIO m) => Integer -> Text -> View -> m ()
addResult = undefined

removeResult :: (MonadIO m) => Text -> View -> m ()
removeResult = undefined

clearResults :: (MonadIO m) => View -> m ()
clearResults = undefined

