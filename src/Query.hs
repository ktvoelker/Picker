
module Query where

import Prelude hiding (FilePath)

import Control.Monad.Trans
import qualified Data.Text as T
import Filesystem.Path ()

import Query.Types

-- TODO
evalQuery :: (MonadIO m) => Query -> (T.Text -> IO ()) -> m ()
evalQuery (QText xs _) cb = liftIO $ mapM_ cb $ drop 1 $ T.inits xs
evalQuery _ _ = undefined

-- TODO
parseQuery :: T.Text -> Query
parseQuery = flip QText $ TextQual False False

