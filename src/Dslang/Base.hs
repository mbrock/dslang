module Dslang.Base
  ( module Dslang.Base
  , module X
  ) where

import Prelude as X
  ( Read (..), Show (..), read
  , Eq (..), Ord (..)
  , Functor (..)
  , Applicative (..)
  , Monad (..)
  , Bool (..)
  , Maybe (..)
  , Either (..)
  , IO
  , String
  , Integer
  , (.)
  , ($)
  , const
  , flip
  , map
  , elem
  , print
  , putStrLn
  , error
  , (+), (-), (*)
  , fromIntegral
  )

import Control.Applicative as X
import Control.Lens as X hiding (universe, noneOf)
import Control.Monad as X
import Control.Monad.Except as X
import Control.Monad.State as X
import Data.Data as X (Data)
import Data.Foldable as X (Foldable, foldr, foldl', toList)
import Data.Monoid as X
import Data.Text as X (Text, pack, unpack, isSuffixOf)
import Data.Text.Encoding as X (encodeUtf8)
import Data.Maybe as X (fromJust)
import Data.Map.Strict as X (Map)
import Data.List as X (sort, sortBy, filter, concat, reverse, elemIndex)
import Data.Function as X (on)
import Data.Void as X
import Data.Char as X (toUpper)
import Data.Set as X (Set)
import Text.Printf as X (printf)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

throw :: MonadError e m => e -> m a
throw = throwError
