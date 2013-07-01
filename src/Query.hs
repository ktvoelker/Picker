
module Query where

import Prelude hiding (FilePath)

import Control.Monad.Reader
import qualified Data.List as L
import Data.Maybe
import qualified Data.MultiSet as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Filesystem
import Filesystem.Path.CurrentOS hiding (concat)

import MaxN
import Query.Types

data E = E { eQuery :: Query, eCallback :: T.Text -> IO () }

type M = ReaderT E IO

isDotFile = ("." `T.isPrefixOf`) . enc . filename

ignoreDirs :: S.Set T.Text
ignoreDirs = S.fromList ["cabal-dev", "dist", "ENV", "_darcs"]

ignoreDir :: FilePath -> Bool
ignoreDir fp = isDotFile fp || (enc . filename $ fp) `S.member` ignoreDirs

binExts :: S.Set T.Text
binExts = S.fromList
  -- Native code
  [ "o", "a", "so", "dylib", "dll", "la"
  -- Other binary executables
  , "hi", "pyc"
  -- Archives and compressed formats
  , "tar", "gz", "xz", "bz2", "zip", "tgz", "tbz", "rar"
  -- Images
  , "jpg", "jpeg", "png", "gif", "bmp", "eps", "cr2", "dng"
  -- Documents
  , "pdf", "odf", "ods", "odp", "ps"
  ]

ignoreFile :: FilePath -> Bool
ignoreFile fp = isDotFile fp || isBinFile
  where
    isBinFile = maybe False (`S.member` binExts) $ extension fp

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred = liftM (f . L.partition snd) . mapM (\x -> liftM (x,) $ pred x)
  where
    f (xs, ys) = (map fst xs, map fst ys)

listFiles :: FilePath -> IO [FilePath]
listFiles dir = if ignoreDir dir then return [] else do
  cs <- listDirectory dir
  (ds, cs') <- partitionM isDirectory cs
  fs <- filterM isFile . filter (not . ignoreFile) $ cs'
  ss <- liftM concat $ mapM listFiles ds
  return $ fs ++ ss

scoreOne :: M.MultiSet T.Text -> T.Text -> (Integer, M.MultiSet T.Text)
scoreOne qs f = (fromIntegral (M.size ys), ns)
  where
    (ys, ns) = M.partition (`T.isInfixOf` f) qs

scoreNull :: M.MultiSet a -> Maybe [b]
scoreNull qs
  | M.null qs = Just []
  | otherwise = Nothing

scoreDir :: M.MultiSet T.Text -> [T.Text] -> Maybe [Integer]
scoreDir qs [] = scoreNull qs
scoreDir qs (d : ds) = fmap (dScore :) dScores
  where
    (dScore, qs') = scoreOne qs d
    dScores = scoreDir qs' ds

score :: Query -> [T.Text] -> Maybe [Integer]
score qs [] = scoreNull qs
score qs (f : ds) = fmap (fScore :) dScores
  where
    (fqs, dqs) = M.partition ((== DirOrFile) . snd) $ qs
    fqs' = M.map fst fqs
    dqs' = M.map fst dqs
    (fScore, fqs'') = scoreOne fqs' f
    dScores = scoreDir (M.union fqs'' dqs') ds

enc = either (error "Unexpected encoding in a FilePath") id . toText

fileAsList :: FilePath -> [T.Text]
fileAsList = map (T.toCaseFold . enc) . reverse . splitDirectories

evalQuery :: (MonadIO m) => Query -> (T.Text -> IO ()) -> m ()
evalQuery q cb = liftIO $ do
  let wd = fromText "."
  files <- listFiles wd
  let strippedFiles = catMaybes . map (stripPrefix wd) $ files
  let scoredFiles = map (\f -> fmap (, f) . score q . fileAsList $ f) strippedFiles
  let maxn = foldr MaxN.insert (MaxN.empty 10) $ catMaybes scoredFiles
  let results = map snd $ filter (any (> 0) . fst) $ MaxN.toList maxn
  mapM_ (cb . enc) results

parseQuery :: T.Text -> Query
parseQuery xs = tokens
  where
    words = T.words . T.toCaseFold $ xs
    tokens = M.fromList . map f $ words
    f token | "/" `T.isSuffixOf` token = (T.init token, DirOnly)
    f token = (token, DirOrFile)
    
debugQuery :: (MonadIO m) => T.Text -> m ()
debugQuery xs = let q = parseQuery xs in liftIO $ print q >> evalQuery q TIO.putStrLn

