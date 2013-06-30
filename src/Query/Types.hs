
module Query.Types where

import Network.Mime
import qualified Data.Set as S
import qualified Data.Text as T

data QueryTokenAttr = DirOnly | DirOrFile deriving (Eq, Ord, Show)

data Query =
  Query
  { qTokens    :: [(T.Text, QueryTokenAttr)]
  , qMimeTypes :: S.Set MimeType
  } deriving (Eq, Show)

