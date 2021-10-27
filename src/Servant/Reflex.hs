{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

-- #include "overlapping-compat.h"
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Reflex
  (
    client
  , clientWithOpts
  , clientWithOptsAndResultHandler
  , KillIdentity(..)
  , NoIdentity
  , idHandlerWrapper
  , module Servant.Common.Req
  , module Servant.Common.BaseUrl
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Functor.Identity
import qualified Data.Map                as Map
import           Data.Monoid             ((<>))
import           Data.Proxy              (Proxy (..))
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as E
import           GHC.Exts                (Constraint)
import           GHC.TypeLits            (KnownSymbol, symbolVal)
import           Servant.API             ((:<|>) (..), (:>), BasicAuth,
                                          BasicAuthData, BuildHeadersTo (..),
                                          Capture, Header, Headers (..),
                                          HttpVersion, IsSecure,
                                          MimeRender (..), MimeUnrender,
                                          NoContent, QueryFlag, QueryParam,
                                          QueryParams, Raw, ReflectMethod (..),
                                          RemoteHost, ReqBody,
                                          ToHttpApiData (..), Vault, Verb,
                                          contentType)
import           Servant.API.Description (Summary)
import qualified Servant.Auth            as Auth

import           Reflex.Dom.Core         (Dynamic, Event, Reflex,
                                          XhrRequest (..), XhrResponse (..),
                                          XhrResponseHeaders (..),
                                          attachPromptlyDynWith, constDyn, ffor,
                                          fmapMaybe, leftmost,
                                          performRequestsAsync)
------------------------------------------------------------------------------
import           Servant.Common.BaseUrl  (BaseUrl(..), Scheme(..), baseUrlWidget,
                                          showBaseUrl,
                                          SupportsServantReflex)
import           Servant.Common.Req      (ClientOptions(..),
                                          defaultClientOptions,
                                          Req, ReqResult(..), QParam(..),
                                          QueryPart(..), addHeader, authData,
                                          defReq, evalResponse, prependToPathParts,
                                          -- performRequestCT,
                                          performRequestsCT,
                                          -- performRequestNoBody,
                                          performRequestsNoBody,
                                          performSomeRequestsAsync,
                                          qParamToQueryPart, reqBody,
                                          reqSuccess, reqFailure,
                                          reqMethod, respHeaders,
                                          response,
                                          reqTag,
                                          qParams, withCredentials)
import           Servant.Reflex.Multi

-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: Event t l -> m (Event t (l, ReqResult [Book]))
-- > postNewBook :: Dynamic t (Maybe Book) -> Event t l
-- >             -> m (Event t (l, ReqResult Book)))
-- > (getAllBooks :<|> postNewBook) = client myApi host
-- >   where host = constDyn $ BaseUrl Http "localhost" 8080

client
    :: ( HasClientMulti t m layout Identity tag, Applicative (Dynamic t)
       , Functor (Event t), Applicative m)
    => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t BaseUrl
    -> ClientMulti t m layout Identity tag
client l m tag baseurl = clientWithOpts l m tag baseurl defaultClientOptions

clientWithOpts
    :: ( HasClientMulti t m layout Identity tag, Applicative (Dynamic t)
       , Functor (Event t), Applicative m)
    => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> ClientMulti t m layout Identity tag
clientWithOpts l m tag url opts =
  clientWithOptsAndResultHandler l m tag url opts pure

-- | Like 'clientWithOpts' but allows passing a function which will process the
-- result event in some way. This can be used to handle errors in a uniform way
-- across call sites.
clientWithOptsAndResultHandler
    :: ( HasClientMulti t m layout Identity tag, Applicative (Dynamic t)
       , Functor (Event t), Functor m)
    => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> (forall a. Event t (ReqResult tag a) -> m (Event t (ReqResult tag a)))
    -> ClientMulti t m layout Identity tag
clientWithOptsAndResultHandler l m tag url opts wrap =
  clientWithRouteMulti l m (Proxy :: Proxy Identity) tag (pure $ Identity defReq)
  url opts (idHandlerWrapper wrap)


idHandlerWrapper
  :: forall t m tag a
  .  (Functor (Event t), Functor m)
  => (Event t (ReqResult tag a) -> m (Event t (ReqResult tag a)))
  -> Event t (Identity (ReqResult tag a)) -> m (Event t (Identity (ReqResult tag a)))
idHandlerWrapper f req = fmap (fmap Identity) $ f $ fmap runIdentity req

class (b ~ NoIdentity t m a) => KillIdentity t m a b where
  killIdentity :: a -> NoIdentity t m a

instance ( KillIdentity t m a na, KillIdentity t m b nb
         ) => KillIdentity t m (a :<|> b) (na :<|> nb) where
  killIdentity (a :<|> b) = killIdentity @t @m a :<|> killIdentity @t @m b

instance (KillIdentity t m b nb) => KillIdentity t m (Identity a -> b) (a -> nb) where
  killIdentity f a = killIdentity @t @m $ f (Identity a)

instance ( KillIdentity t m b nb, Functor (Dynamic t)
         ) => KillIdentity t m (Dynamic t (Identity a) -> b) (Dynamic t a -> nb) where
  killIdentity f da = killIdentity @t @m $ f $ Identity <$> da

instance ( NoIdentity t m (a -> b) ~ (a -> nb), KillIdentity t m b nb
         ) => KillIdentity t m (a -> b) (a -> nb) where
  killIdentity f a = killIdentity @t @m $ f a

instance ( Functor m, Functor (Event t)
         , NoIdentity t m (m (Event t (Identity a))) ~ (m (Event t a))
         ) => KillIdentity t m (m (Event t (Identity a))) (m (Event t a)) where
  killIdentity = fmap (fmap runIdentity)

type family NoIdentity t m a where
  NoIdentity t m (a :<|> b) = NoIdentity t m a :<|> NoIdentity t m b
  NoIdentity t m (Identity a -> b) = a -> NoIdentity t m b
  NoIdentity t m (Dynamic t (Identity a) -> b) = Dynamic t a -> NoIdentity t m b
  NoIdentity t m (a -> b) = a -> NoIdentity t m b
  NoIdentity t m (m (Event t (Identity a))) = m (Event t a)
