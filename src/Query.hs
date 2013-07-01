
module Query where

import Prelude hiding (FilePath)

import Control.Monad.Reader
import qualified Data.List as L
import Data.Maybe
import qualified Data.MultiSet as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Filesystem
import Filesystem.Path.CurrentOS hiding (concat)

import MaxN
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

scoreOne :: M.MultiSet T.Text -> T.Text -> (Integer, M.MultiSet T.Text)
scoreOne qs f = (fromIntegral (M.size ys), ns)
  where
    (ys, ns) = M.partition (`T.isInfixOf` f) qs

scoreDir :: M.MultiSet T.Text -> [T.Text] -> [Integer]
scoreDir _ [] = []
scoreDir qs (d : ds) = dScore : dScores
  where
    (dScore, qs') = scoreOne qs d
    dScores = scoreDir qs' ds

score :: M.MultiSet (T.Text, QueryTokenAttr) -> [T.Text] -> [Integer]
score _ [] = []
score qs (f : ds) = fScore : dScores
  where
    (fqs, dqs) = M.partition ((== DirOrFile) . snd) $ qs
    fqs' = M.map fst fqs
    dqs' = M.map fst dqs
    (fScore, fqs'') = scoreOne fqs' f
    dScores = scoreDir (M.union fqs'' dqs') ds

enc = either (error "Unexpected encoding in a FilePath") id . toText

fileAsList :: FilePath -> [T.Text]
fileAsList = map (T.toLower . enc) . reverse . splitDirectories

evalQuery :: (MonadIO m) => Query -> (T.Text -> IO ()) -> m ()
evalQuery q cb = liftIO $ do
  let wd = fromText "."
  files <- listFiles wd
  let strippedFiles = catMaybes . map (stripPrefix wd) $ files
  let scoredFiles = map (\f -> (score (qTokens q) (fileAsList f), f)) strippedFiles
  let maxn = foldr MaxN.insert (MaxN.empty 10) scoredFiles
  let results = map snd $ filter (any (> 0) . fst) $ MaxN.toList maxn
  -- TODO if a mime-type filter was given, apply it here
  mapM_ (cb . enc) results

parseQuery :: T.Text -> Query
parseQuery xs = Query tokens mimeTypes
  where
    words = T.words . T.toLower $ xs
    (rawMimeTypes, rawTokens) = L.partition (":" `T.isPrefixOf`) words
    tokens = M.fromList . map f $ rawTokens
    f token | "/" `T.isSuffixOf` token = (T.init token, DirOnly)
    f token = (token, DirOrFile)
    mimeTypes = S.fromList . map TE.encodeUtf8 $ rawMimeTypes
    
debugQuery :: (MonadIO m) => T.Text -> m ()
debugQuery xs = let q = parseQuery xs in liftIO $ print q >> evalQuery q TIO.putStrLn

