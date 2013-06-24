
module Query.Types where

import Network.Mime
import Data.Text

data BinOp = And | Or deriving (Show)

data TextQual =
  TextQual
  { tqInRoot :: Bool
  , tqIsDir  :: Bool
  } deriving (Show)

data Query =
    QBinOp BinOp Query Query
  | QNot Query
  | QConst Bool
  | QText Text TextQual
  | QMime MimeType
  deriving (Show)

