
{-# LANGUAGE BangPatterns #-}
module View
  ( View()
  , ViewOptions(..)
  , startView
  , stopView
  , waitForView
  , addResult
  , clearResults
  ) where

import Prelude hiding (FilePath, (.))

import Control.Category
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Lens hiding (focus)
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput
import Graphics.Vty.Widgets.All
import View.Types

startView :: (MonadIO m) => ViewOptions -> m View
startView opts = liftIO $ do
  (root, fg, mkView) <- makeWidgets opts
  coll <- newCollection
  void $ addToCollection coll root fg
  stopped <- newEmptyMVar
  let view = mkView stopped
  addListeners view
  void $ forkIO $ do
    runUi coll defaultContext
    putMVar stopped ()
  return view

waitForView :: (MonadIO m) => View -> m ()
waitForView = liftIO . takeMVar . _vStopped

stopView :: (MonadIO m) => View -> Maybe T.Text -> m ()
stopView view result = liftIO $ schedule $ do
  (voResultHandler . vOpts ^$ view) $ result
  shutdownUi

makeWidgets opts = do
  title   <- plainText "Picker"
  query   <- editWidget
  -- TODO pass different attrs here - it's for the selected item
  results <- newTextList def_attr []
  scroll  <- newProgressBar def_attr def_attr
  top     <- vBox title query
  bottom  <- hBox results scroll
  all     <- vBox top bottom
  fg      <- newFocusGroup
  void $ addToFocusGroup fg query
  setFocusGroupNextKey fg KDown []
  setFocusGroupPrevKey fg KUp []
  return (all, fg, View opts query results scroll)

addListeners view = do
  let opts    = vOpts ^$ view
  let query   = vQuery ^$ view
  let results = vResults ^$ view
  onChange query $ voQueryHandler ^$ opts
  onItemActivated results $ stopView view . Just . evText
  onActivate query $ const $ activateCurrentItem results
  onKeyPressed query $ \_ key _ -> do
    case key of
      KEsc -> stopView view Nothing >> return True
      KASCII '\t' -> do
        n  <- getListSize results
        mi <- getSelected results
        case mi of
          _ | n == 0 -> return ()
          Nothing -> setSelected results 0
          Just (i, _) | i + 1 == n -> setSelected results 0
          Just (i, _) -> setSelected results (i + 1)
        return True
      _ -> return False
  where
    evText (ActivateItemEvent _ xs _) = xs

addResult :: (MonadIO m) => View -> T.Text -> m ()
addResult View{ _vResults = !rs } !xs = liftIO $ do
  tw <- plainText xs
  schedule $ do
    addToList rs xs tw
    getSelected rs >>= maybe (setSelected rs 0) (const $ return ())

clearResults :: (MonadIO m) => View -> m ()
clearResults View{ _vResults = !rs } = liftIO $ clearList rs

