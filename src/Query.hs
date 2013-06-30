
module Query where

import Prelude hiding (FilePath)

import Control.Monad.Reader
import qualified Data.List as L
import Data.Maybe
import qualified Data.MultiSet as M
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
partitionM pred = liftM (f . L.partition snd) . mapM (\x -> liftM (x,) $ pred x)
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
score :: M.MultiSet (T.Text, QueryTokenAttr) -> [T.Text] -> [Integer]
score = undefined

enc = either (error "Unexpected encoding in a FilePath") id . toText

fileAsList :: FilePath -> [T.Text]
fileAsList = map enc . reverse . splitDirectories

evalQuery :: (MonadIO m) => Query -> (T.Text -> IO ()) -> m ()
evalQuery q cb = liftIO $ do
  let wd = fromText "."
  files <- listFiles wd
  let strippedFiles = catMaybes . map (stripPrefix wd) $ files
  let scoredFiles = map (\f -> (score (qTokens q) (fileAsList f), f)) strippedFiles
  let results = map snd $ L.sort $ filter (any (> 0) . fst) $ scoredFiles
  mapM_ (cb . enc) results

parseQuery :: T.Text -> Query
parseQuery xs = Query tokens mimeTypes
  where
    (rawTokens, rawMimeTypes) = L.partition (":" `T.isPrefixOf`) . T.words $ xs
    tokens = M.fromList . map f $ rawTokens
    f token | "/" `T.isSuffixOf` token = (T.init token, DirOnly)
    f token = (token, DirOrFile)
    mimeTypes = S.fromList . map TE.encodeUtf8 $ rawMimeTypes
    

