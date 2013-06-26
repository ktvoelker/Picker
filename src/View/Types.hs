
{-# LANGUAGE TemplateHaskell #-}
module View.Types where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Monad.State
import Data.Lens.Template
import qualified Data.MeldableHeap as PQ
import qualified Data.Text as T
import Graphics.Vty
import Filesystem.Path

data ViewCommand =
    AddResult !Integer !T.Text
  | ClearResults
  | Shutdown
  deriving (Eq, Show)

newtype ViewHandle = ViewHandle { getChan :: Chan ViewCommand }

data View =
  View
  { _vOpts       :: ViewOptions
  , _vResults    :: PQ.PQ (Integer, FilePath)
  , _vChan       :: Chan ViewCommand
  , _vScreen     :: Vty
  }

data ViewOptions =
  ViewOptions
  { _voQueryHandler  :: T.Text -> IO ()
  , _voResultHandler :: FilePath -> IO ()
  }

type M a = StateT View IO a

makeLenses [''View, ''ViewOptions]

