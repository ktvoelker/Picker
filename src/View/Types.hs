
{-# LANGUAGE TemplateHaskell #-}
module View.Types where

import Prelude hiding (FilePath)

import Data.Lens.Template
import qualified Data.Text as T
import Graphics.Vty.Widgets.All

data View =
  View
  { _vOpts    :: ViewOptions
  , _vQuery   :: Widget Edit
  , _vResults :: Widget (List T.Text FormattedText)
  , _vScroll  :: Widget ProgressBar
  }

data ViewOptions =
  ViewOptions
  { _voQueryHandler  :: T.Text -> IO ()
  , _voResultHandler :: T.Text -> IO ()
  }

makeLenses [''View, ''ViewOptions]

