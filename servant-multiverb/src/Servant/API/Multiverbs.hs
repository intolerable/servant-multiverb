module Servant.API.Multiverbs
  ( Verbs
  , (:\/)
  , Result(..)
  , Deletes
  , Gets
  , Patches
  , Posts
  , Puts
  , Servant.API.Verbs.ReflectMethod
  , Servant.API.Verbs.StdMethod
  ) where

import Data.Functor.Identity
import Data.Kind
import GHC.TypeLits
import Network.HTTP.Types
import Servant.Links
import qualified Servant.API.Verbs

-- | Synonym for 'Either' since we'd prefer not to have nested
--     'Either's in our API type
type a :\/ b = Either a b

data Verbs (method :: k1) (contentTypes :: [Type]) (a :: k2)

newtype Result (n :: Nat) a = Result a
  deriving (Show, Read, Eq, Ord)
  deriving (Functor, Applicative, Monad) via Identity

type Deletes = Verbs 'DELETE
type Gets = Verbs 'GET
type Patches = Verbs 'PATCH
type Posts = Verbs 'POST
type Puts = Verbs 'PUT

instance HasLink (Verbs m ctypes alt) where
  type MkLink (Verbs m ctypes alt) r = r
  toLink mk _ = mk

-- TODO: IsElem instance
