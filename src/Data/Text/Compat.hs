module Data.Text.Compat
  ( show
  ) where

import qualified Data.Text as T
import qualified Prelude as P

show :: P.Show a => a -> T.Text
show = T.pack P.. P.show
