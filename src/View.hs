
{-# LANGUAGE BangPatterns #-}
module View
  ( View()
  , ViewOptions(..)
  , startView
  , waitForView
  , addResult
  , clearResults
  ) where

import Prelude hiding (FilePath)

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
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
  void $ forkIO $ do
    runUi coll defaultContext
    putMVar stopped ()
  return $ mkView stopped

waitForView :: (MonadIO m) => View -> m ()
waitForView = liftIO . takeMVar . _vStopped

makeWidgets opts = do
  title   <- plainText "Picker"
  query   <- editWidget
  onChange query $ _voQueryHandler opts
  results <- newTextList def_attr []
  onItemActivated results $ _voResultHandler opts . Just . evText
  onActivate query $ const $ activateCurrentItem results
  onKeyPressed query $ \_ key _ -> do
    case key of
      KEsc -> do
        _voResultHandler opts Nothing
        shutdownUi
        return True
      KASCII '\t' -> do
        n  <- getListSize results
        mi <- getSelected results
        case mi of
          _ | n == 0 -> return ()
          Nothing -> setSelected results 0
          Just (i, _) | i == n -> setSelected results 0
          Just (i, _) -> setSelected results (i + 1)
        return True
      _ -> return False
  scroll  <- newProgressBar def_attr def_attr
  top     <- vBox title query
  bottom  <- hBox results scroll
  all     <- vBox top bottom
  fg      <- newFocusGroup
  void $ addToFocusGroup fg query
  return (all, fg, View opts query results scroll)
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

