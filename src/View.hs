
{-# LANGUAGE BangPatterns #-}
module View where

import Prelude hiding (FilePath)

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Graphics.Vty.Widgets.All
import View.Types

runView :: (MonadIO m) => ViewOptions -> ReaderT View m a -> m a
runView opts rdr = do
  view <- liftIO $ do
    (root, fg, view) <- makeWidgets opts
    coll <- newCollection
    void $ addToCollection coll root fg
    void $ forkIO $ runUi coll defaultContext
    return view
  runReaderT rdr view

makeWidgets opts = do
  title   <- plainText "Picker"
  query   <- editWidget
  results <- newTextList def_attr []
  scroll  <- newProgressBar def_attr def_attr
  top     <- vBox title query
  bottom  <- hBox results scroll
  all     <- vBox top bottom
  fg      <- newFocusGroup
  void $ addToFocusGroup fg query
  return (all, fg, View opts query results scroll)

addResult :: (MonadIO m) => T.Text -> ReaderT View m ()
addResult !xs = do
  rs <- asks _vResults
  liftIO $ do
    tw <- plainText xs
    schedule $ addToList rs xs tw

clearResults :: (MonadIO m) => ReaderT View m ()
clearResults = asks _vResults >>= liftIO . clearList

