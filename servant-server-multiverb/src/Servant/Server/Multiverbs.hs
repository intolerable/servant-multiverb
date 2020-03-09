{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Server.Multiverbs
  ( module Servant.API.Multiverbs
  , module Servant.Server.Multiverbs
  ) where

import Servant.API.ResponseHeaders
import Servant.API.Multiverbs
import Servant.Server.Internal
import Servant
import Servant.API.ContentTypes
import Data.Kind
import Data.Maybe
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.Wai
import Network.HTTP.Types
import GHC.TypeLits
import qualified Data.ByteString.Lazy.Char8 as ByteString

class VerbsHandler a where
  type HandlerType a :: Type

  findStatus :: Proxy a -> HandlerType a -> Status

instance {-# OVERLAPPABLE #-} KnownNat s => VerbsHandler (Result s a) where
  type HandlerType (Result s a) = a
  findStatus _ _ = toEnum $ fromInteger $ natVal (Proxy @s)

instance {-# OVERLAPPING #-} (VerbsHandler a, VerbsHandler b) => VerbsHandler (a :<|> b) where
  type HandlerType (a :<|> b) = Either (HandlerType a) (HandlerType b)
  findStatus _ = \case
    Left a -> findStatus (Proxy @a) a
    Right b -> findStatus (Proxy @b) b

class AllMime ctypes => AllCTRender1 ctypes a where
  handleAcceptH1 :: Proxy ctypes -> AcceptHeader -> a -> Maybe (ByteString, ResponseHeaders, ByteString)

instance {-# OVERLAPS #-} (AllMime ctypes, AllCTRender ctypes a) => AllCTRender1 ctypes a where
  handleAcceptH1 p accept v = do
    (ct, body) <- handleAcceptH p accept v
    pure (ct, [], body)

instance {-# OVERLAPS #-} (AllMime ctypes, AllCTRender1 ctypes a, GetHeaders (Headers hs a)) => AllCTRender1 ctypes (Headers hs a) where
  handleAcceptH1 p accept v = do
    (ct, hs, body) <- handleAcceptH1 p accept (getResponse v)
    pure (ct, getHeaders v <> hs, body)

instance {-# OVERLAPS #-} (AllCTRender1 ctypes a, AllCTRender1 ctypes b) => AllCTRender1 ctypes (Either a b) where
  handleAcceptH1 p accept = \case
    Left a -> handleAcceptH1 p accept a
    Right b -> handleAcceptH1 p accept b

instance {-# OVERLAPPABLE #-} (VerbsHandler alt, AllCTRender1 ctypes (HandlerType alt), ReflectMethod method) =>
  HasServer (Verbs method ctypes alt) context where
  type ServerT (Verbs method ctypes alt) m = m (HandlerType alt)

  hoistServerWithContext _p _ctx nt a = nt a

  route _p _c action = leafRouter route'
    where
      route' env req resp = do
        let acceptHeader = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders req
        let method = reflectMethod (Proxy @method)
        let finalAction =
              action
                `addMethodCheck` methodCheck method req
                `addAcceptCheck` acceptCheck (Proxy @ctypes) acceptHeader
        runAction finalAction env req resp $ \res -> do
          case handleAcceptH1 (Proxy @ctypes) (AcceptHeader acceptHeader) res of
            Nothing -> FailFatal err406
            Just (ctype, headers, body) -> do
              let actualBody = if allowedMethodHead method req then "" else body
              Route $ responseLBS (findStatus (Proxy @alt) res) ((hContentType, ByteString.toStrict ctype) : headers) actualBody
