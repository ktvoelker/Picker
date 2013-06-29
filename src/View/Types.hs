
{-# LANGUAGE TemplateHaskell #-}
module View.Types where

import Prelude hiding (FilePath)

import Control.Concurrent.MVar
import Data.Lens.Template
import qualified Data.Text as T
import Graphics.Vty.Widgets.All

data View =
  View
  { _vOpts    :: ViewOptions
  , _vQuery   :: Widget Edit
  , _vResults :: Widget (List T.Text FormattedText)
  , _vScroll  :: Widget ProgressBar
  , _vStopped :: MVar ()
  }

data ViewOptions =
  ViewOptions
  { _voQueryHandler  :: T.Text -> IO ()
  , _voResultHandler :: Maybe T.Text -> IO ()
  }

makeLenses [''View, ''ViewOptions]

