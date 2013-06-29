
module Query where

import Prelude hiding (FilePath)

import Control.Monad.Reader
import qualified Data.Text as T
import Filesystem
import Filesystem.Path.CurrentOS

import Query.Types

data E = E { eQuery :: Query, eCallback :: T.Text -> IO () }

type M = ReaderT E IO

-- TODO
evalDir' :: Query -> FilePath -> Bool
evalDir' = undefined

evalDir :: FilePath -> M ()
evalDir fp = do
  q <- asks eQuery
  when (evalDir' q fp) $ liftIO (listDirectory fp) >>= mapM_ evalAny

-- TODO
evalFile' :: Query -> FilePath -> Bool
evalFile' = undefined

evalFile :: FilePath -> M ()
evalFile fp = do
  q <- asks eQuery
  when (evalFile' q fp) $ asks eCallback >>= liftIO . ($ fp')
  where
    fp' = either (error "Unexpected encoding in a FilePath") id $ toText fp

evalAny :: FilePath -> M ()
evalAny fp = liftM2 (,) (liftIO $ isDirectory fp) (liftIO $ isFile fp) >>= \case
  (True, False) -> evalDir fp
  (False, True) -> evalFile fp
  _             -> return ()

runM :: Query -> (T.Text -> IO ()) -> M a -> IO a
runM q cb m = runReaderT m (E q cb)

evalQuery :: (MonadIO m) => Query -> (T.Text -> IO ()) -> m ()
evalQuery q cb = liftIO $ getWorkingDirectory >>= runM q cb . evalAny

-- TODO
parseQuery :: T.Text -> Query
parseQuery = flip QText $ TextQual False False

