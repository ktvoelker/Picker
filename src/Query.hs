
module Query where

import Prelude hiding (FilePath)

import Control.Monad.Reader
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Filesystem
import Filesystem.Path.CurrentOS hiding (concat)

import Query.Types

data E = E { eQuery :: Query, eCallback :: T.Text -> IO () }

type M = ReaderT E IO

ignoreDir :: FilePath -> IO Bool
ignoreDir = const $ return False

-- TODO
ignoreFile :: FilePath -> IO Bool
ignoreFile = const $ return False

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred = liftM (f . partition snd) . mapM (\x -> liftM (x,) $ pred x)
  where
    f (xs, ys) = (map fst xs, map fst ys)

listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
  ignoreDir dir >>= \case
    True -> return []
    False -> do
      cs <- listDirectory dir
      (ds, cs') <- partitionM isDirectory cs
      fs <- filterM isFile cs'
      ss <- liftM concat $ mapM listFiles ds
      return $ fs ++ ss

-- TODO
score :: Query -> FilePath -> [Integer]
score = undefined

enc = either (error "Unexpected encoding in a FilePath") id . toText

evalQuery :: (MonadIO m) => Query -> (T.Text -> IO ()) -> m ()
evalQuery q cb = liftIO $ do
  fs <- getWorkingDirectory >>= listFiles
  let ss = map snd $ sort $ filter (any (> 0) . fst) $ map (\f -> (score q f, f)) fs
  mapM_ (cb . enc) ss

parseQuery :: T.Text -> Query
parseQuery xs = Query tokens mimeTypes
  where
    (rawTokens, rawMimeTypes) = partition (":" `T.isPrefixOf`) . T.words $ xs
    tokens = map f rawTokens
    f token | "/" `T.isSuffixOf` token = (T.init token, DirOnly)
    f token = (token, DirOrFile)
    mimeTypes = S.fromList . map TE.encodeUtf8 $ rawMimeTypes
    

