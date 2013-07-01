
module Query.Types where

import qualified Data.MultiSet as M
import qualified Data.Text as T

data QueryTokenAttr = DirOnly | DirOrFile deriving (Eq, Ord, Show)

type Query = M.MultiSet (T.Text, QueryTokenAttr)

