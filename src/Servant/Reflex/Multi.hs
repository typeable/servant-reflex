{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.Reflex.Multi (
    -- -- * Compute servant client functions
    --   clientA
    -- , clientWithOptsA
    -- , clientWithOptsAndResultHandlerA
    -- , BaseUrl(..)
    -- , Scheme(..)

    -- -- * Build QueryParam arguments
    -- , QParam(..)

    -- -- * Access response data
    -- , withCredentials

    -- -- * Access response data
    -- , ReqResult(..)
    -- , reqSuccess
    -- , reqSuccess'
    -- , reqFailure
    -- , response
    -- , BuildHeaderKeysTo(..)
    -- , toHeaders
    -- , HasClientMulti(..)
    ) where

------------------------------------------------------------------------------
import           Control.Applicative     (liftA2)
import           Data.Functor.Compose    (Compose (..), getCompose)
import qualified Data.CaseInsensitive    as CI
import           Data.Proxy              (Proxy (..))
import           Data.Kind (Type)
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as E
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

import           Reflex.Dom.Core         (Dynamic, Event, Reflex,
                                          XhrRequest (..),
                                          XhrResponseHeaders (..),
                                          attachPromptlyDynWith, constDyn)
import           Reflex.Dom.Core         (Dynamic, Event, Reflex,
                                          XhrRequest (..), XhrResponse (..),
                                          XhrResponseHeaders (..),
                                          attachPromptlyDynWith, constDyn, ffor,
                                          fmapMaybe, leftmost,
                                          performRequestsAsync)
------------------------------------------------------------------------------
import           Servant.Common.BaseUrl (BaseUrl (..), Scheme (..),
                                         SupportsServantReflex)
import           Servant.Common.Req     (ClientOptions,
                                         QParam (..), QueryPart (..), Req,
                                         ReqResult (..), addHeader, authData,
                                         defReq,
                                         defaultClientOptions,
                                         performRequestsCT,
                                         performRequestsNoBody,
                                         performSomeRequestsAsync,
                                         prependToPathParts, qParamToQueryPart,
                                         qParams, reqBody, reqFailure,
                                         reqMethod, reqSuccess, reqSuccess',
                                         respHeaders, response, withCredentials)


-- -- ------------------------------------------------------------------------------
-- clientA
--   :: (HasClientMulti t m layout f tag, Applicative f, Reflex t, Applicative m)
--   => Proxy layout
--   -> Proxy m
--   -> Proxy f
--   -> Proxy tag
--   -> Dynamic t BaseUrl
--   -> ClientMulti t m layout f tag
-- clientA p q f tag baseurl  =
--   clientWithOptsA p q f tag baseurl defaultClientOptions

-- clientWithOptsA
--   :: (HasClientMulti t m layout f tag, Applicative f, Reflex t, Applicative m)
--   => Proxy layout
--   -> Proxy m
--   -> Proxy f
--   -> Proxy tag
--   -> Dynamic t BaseUrl
--   -> ClientOptions
--   -> ClientMulti t m layout f tag
-- clientWithOptsA l m f tag url opts =
--   clientWithOptsAndResultHandlerA l m f tag url opts pure

-- -- | A version of @client@ that sets the withCredentials flag
-- -- on requests. Use this function for clients of CORS API's
-- clientWithOptsAndResultHandlerA
--   :: (HasClientMulti t m layout f tag, Applicative f, Reflex t)
--   => Proxy layout
--   -> Proxy m
--   -> Proxy f
--   -> Proxy tag
--   -> Dynamic t BaseUrl
--   -> ClientOptions
--   -> (forall a. Event t (f (ReqResult tag a)) -> m (Event t (f (ReqResult tag a))))
--   -> ClientMulti t m layout f tag
-- clientWithOptsAndResultHandlerA p q f tag baseurl opts wrap =
--     clientWithRouteMulti p q f tag
--     (constDyn (pure defReq)) baseurl opts wrap

type family FApply f a where
  FApply 'Nothing a = a
  FApply ('Just f) a = f a

class (fa ~ FApply f a) => MPure (f :: Maybe (Type -> Type)) a fa | f a -> fa, fa a -> f, fa f -> a where
  mpure :: a -> fa

instance MPure 'Nothing a a where
  mpure a = a

instance (Applicative f) => MPure ('Just f) a (f a) where
  mpure = pure

class MApply (f :: Maybe (Type -> Type)) fab fa fb
  | f fab -> fa fb, f fa fb -> fab, fab fa fb -> f where
  mapply :: fab -> fa -> fb

instance MApply 'Nothing (a -> b) a b where
  mapply f a = f a

instance (Applicative f) => MApply ('Just f) (f (a -> b)) (f a) (f b) where
  mapply = (<*>)

class (FApply f a ~ fa, FApply f b ~ fb) => MTraverse (f :: Maybe (Type -> Type)) a b fa fb
  | f a -> fa, fa a -> f, fa f -> a
  , f b -> fb, fb b -> f, fb f -> b where
  mtraverse :: forall m. (Applicative m) => (a -> m b) -> fa -> m fb

instance MTraverse 'Nothing a b a b where
  mtraverse f a = f a

instance (Traversable f) => MTraverse ('Just f) a b (f a) (f b) where
  mtraverse = traverse

-- | Maybe Traversable Applicative functor @f@ applied to @a@ and a function @a -> a@
type MTravApp f a fa faa =
  ( MPure f a fa
  , MApply f faa fa fa
  , MTraverse f a a fa fa
  )

mfmap
  :: forall f a b fa fb fab
  .  (MPure f (a -> b) fab, MApply f fab fa fb)
  => (a -> b) -> fa -> fb
mfmap f a = mapply (mpure @f f) a

------------------------------------------------------------------------------
class HasClientMulti t m layout (f :: Maybe (Type -> Type)) (tag :: *) where
  type ClientMulti t m layout f tag :: *
  clientWithRouteMulti
    :: forall fReq fReqReq
    .  (MTravApp f (Req t) fReq fReqReq)
    => Proxy layout
    -> Proxy m
    -> Proxy f
    -> Proxy tag
    -> Dynamic t fReq
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> Wrapper t m f tag
    -> ClientMulti t m layout f tag

newtype Wrapper t m f tag = Wrapper
  { unWrapper
    :: forall a fRes fResRes
    .  (MTravApp f (ReqResult tag a) fRes fResRes)
    => Event t fRes
    -> m (Event t fRes)
  }

------------------------------------------------------------------------------
instance (HasClientMulti t m a f tag, HasClientMulti t m b f tag) =>
    HasClientMulti t m (a :<|> b) f tag where
  type ClientMulti t m (a :<|> b) f tag = ClientMulti t m a f tag :<|>
                                          ClientMulti t m b f tag
  clientWithRouteMulti Proxy q f tag reqs baseurl opts wrap =
    clientWithRouteMulti (Proxy :: Proxy a) q f tag reqs baseurl opts wrap :<|>
    clientWithRouteMulti (Proxy :: Proxy b) q f tag reqs baseurl opts wrap


------------------------------------------------------------------------------
instance
  ( SupportsServantReflex t m
  , ToHttpApiData a
  , HasClientMulti t m sublayout f tag
  ) => HasClientMulti t m (Capture capture a :> sublayout) f tag where

  type ClientMulti t m (Capture capture a :> sublayout) f tag =
    FApply f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f tag

  -- clientWithRouteMulti ::
  --   ::l Proxy layout
  --   -> Proxy m
  --   -> Proxy f
  --   -> Proxy tag
  --   -> Dynamic t fReq
  --   -> Dynamic t BaseUrl
  --   -> ClientOptions
  --   -> Wrapper t m f tag
  --   -> ClientMulti t m layout f tag
  clientWithRouteMulti _ q f tag reqs baseurl opts wrap vals =
    clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl opts wrap
    where
      reqs' = (mapply @f (mfmap @f prependToPathParts ps)) <$> reqs
      ps    = (mfmap .  fmap . fmap) toUrlPiece vals


-- ------------------------------------------------------------------------------
-- -- VERB (Returning content) --
-- instance {-# OVERLAPPABLE #-}
--   -- Note [Non-Empty Content Types]
--   (MimeUnrender ct a,
--    ReflectMethod method, cts' ~ (ct ': cts),
--    SupportsServantReflex t m,
--    Applicative f,
--    Traversable f
--   ) => HasClientMulti t m (Verb method status cts' a) f tag where

--   type ClientMulti t m (Verb method status cts' a) f tag =
--     Event t tag -> m (Event t (f (ReqResult tag a)))

--   clientWithRouteMulti _ _ _ _ reqs baseurl opts wrap trigs =
--     wrap =<< performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl opts trigs
--       where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
--             reqs' = fmap (\r -> r { reqMethod = method }) <$> reqs


-- ------------------------------------------------------------------------------
-- -- -- VERB (No content) --
-- instance {-# OVERLAPPING #-}
--   (ReflectMethod method, SupportsServantReflex t m, Traversable f) =>
--   HasClientMulti t m (Verb method status cts NoContent) f tag where
--   type ClientMulti t m (Verb method status cts NoContent) f tag =
--     Event t tag -> m (Event t (f (ReqResult tag NoContent)))
--     -- TODO: how to access input types here?
--     -- ExceptT ServantError IO NoContent
--   clientWithRouteMulti Proxy _ _ _ req baseurl opts wrap trigs =
--     wrap =<< performRequestsNoBody method req baseurl opts trigs
--       where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)


-- ------------------------------------------------------------------------------
-- instance {-# OVERLAPPABLE #-}
--   -- Note [Non-Empty Content Types]
--   ( MimeUnrender ct a, BuildHeadersTo ls, BuildHeaderKeysTo ls,
--     ReflectMethod method, cts' ~ (ct ': cts),
--     SupportsServantReflex t m,
--     Traversable f
--   ) => HasClientMulti t m (Verb method status cts' (Headers ls a)) f tag where
--   type ClientMulti t m (Verb method status cts' (Headers ls a)) f tag =
--     Event t tag -> m (Event t (f (ReqResult tag (Headers ls a))))
--   clientWithRouteMulti Proxy _ _ _ reqs baseurl opts wrap triggers = do
--     let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
--     resp <- wrap =<< performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl opts triggers :: m (Event t (f (ReqResult tag a)))
--     return $ fmap toHeaders <$> resp
--     where
--       reqs' = fmap (\r ->
--                 r { respHeaders =
--                     OnlyHeaders (Set.fromList
--                                  (buildHeaderKeysTo (Proxy :: Proxy ls)))
--                   }) <$> reqs


-- ------------------------------------------------------------------------------
-- instance {-# OVERLAPPABLE #-}
--   ( BuildHeadersTo ls,
--     BuildHeaderKeysTo ls,
--     ReflectMethod method,
--     SupportsServantReflex t m,
--     Traversable f
--   ) => HasClientMulti t m (Verb method status
--                            cts (Headers ls NoContent)) f tag where
--   type ClientMulti t m (Verb method status cts (Headers ls NoContent)) f tag
--     = Event t tag -> m (Event t (f (ReqResult tag (Headers ls NoContent))))
--   clientWithRouteMulti Proxy _ _ _ reqs baseurl opts wrap triggers = do
--     let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
--     resp <- wrap =<< performRequestsNoBody method reqs' baseurl opts triggers
--     return $ fmap toHeaders <$> resp
--     where reqs' = fmap (\req ->
--                     req {respHeaders = OnlyHeaders (Set.fromList
--                          (buildHeaderKeysTo (Proxy :: Proxy ls)))
--                         }) <$> reqs


-- ------------------------------------------------------------------------------
-- instance (KnownSymbol sym,
--           ToHttpApiData a,
--           HasClientMulti t m sublayout f tag,
--           SupportsServantReflex t m,
--           Traversable f,
--           Applicative f)
--       => HasClientMulti t m (Header sym a :> sublayout) f tag where

--   type ClientMulti t m (Header sym a :> sublayout) f tag =
--     f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f tag

--   clientWithRouteMulti Proxy f q tag reqs baseurl opts wrap eVals =
--     clientWithRouteMulti (Proxy :: Proxy sublayout) f
--                     q tag
--                     reqs'
--                     baseurl opts wrap
--     where hname = T.pack $ symbolVal (Proxy :: Proxy sym)
--           reqs' = ((\eVal req -> Servant.Common.Req.addHeader hname eVal req)
--                   <$> eVals <*>) <$> reqs


-- ------------------------------------------------------------------------------
-- instance HasClientMulti t m sublayout f tag
--   => HasClientMulti t m (HttpVersion :> sublayout) f tag where

--   type ClientMulti t m (HttpVersion :> sublayout) f tag =
--     ClientMulti t m sublayout f tag

--   clientWithRouteMulti Proxy q f tag =
--     clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag


-- ------------------------------------------------------------------------------
-- instance (HasClientMulti t m sublayout f tag, KnownSymbol sym)
--   => HasClientMulti t m (Summary sym :> sublayout) f tag where

--   type ClientMulti t m (Summary sym :> sublayout) f tag =
--     ClientMulti t m sublayout f tag

--   clientWithRouteMulti Proxy q f tag =
--     clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag


-- ------------------------------------------------------------------------------
-- instance (KnownSymbol sym,
--           ToHttpApiData a,
--           HasClientMulti t m sublayout f tag,
--           Reflex t,
--           Applicative f)
--       => HasClientMulti t m (QueryParam sym a :> sublayout) f tag where

--   type ClientMulti t m (QueryParam sym a :> sublayout) f tag =
--     Dynamic t (f (QParam a)) -> ClientMulti t m sublayout f tag

--   -- if mparam = Nothing, we don't add it to the query string
--   -- TODO: Check the above comment
--   clientWithRouteMulti Proxy q f tag reqs baseurl opts wrap mparams =
--     clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag
--       reqs' baseurl opts wrap

--     where pname = symbolVal (Proxy :: Proxy sym)
--           p prm = QueryPartParam $ fmap qParamToQueryPart prm
--           paramPair mp = (T.pack pname, p mp)
--           -- reqs' = (\params reqs -> (\req param -> req {qParams = paramPair param : qParams req}) <$> reqs <*> params)
--           --         <$> mparams <*> reqs
--           reqs' = liftA2 (\(pr :: QParam a) (r :: Req t) -> r { qParams = paramPair (constDyn pr) : qParams r })
--                   <$> mparams <*> reqs


-- instance (KnownSymbol sym,
--           ToHttpApiData a,
--           HasClientMulti t m sublayout f tag,
--           Reflex t,
--           Applicative f)
--       => HasClientMulti t m (QueryParams sym a :> sublayout) f tag where

--   type ClientMulti t m (QueryParams sym a :> sublayout) f tag =
--     Dynamic t (f [a]) -> ClientMulti t m sublayout f tag

--   clientWithRouteMulti Proxy q f tag reqs baseurl opts wrap paramlists =
--     clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl opts wrap

--       where req' l r = r { qParams =  (T.pack pname, params' (constDyn l)) : qParams r }
--             pname   = symbolVal (Proxy :: Proxy sym)
--             params' l = QueryPartParams $ (fmap . fmap) (toQueryParam)
--                         l
--             reqs' = liftA2 req' <$> paramlists <*> reqs


-- instance (KnownSymbol sym,
--           HasClientMulti t m sublayout f tag,
--           Reflex t,
--           Applicative f)
--       => HasClientMulti t m (QueryFlag sym :> sublayout) f tag where

--   type ClientMulti t m (QueryFlag sym :> sublayout) f tag =
--     Dynamic t (f Bool) -> ClientMulti t m sublayout f tag

--   clientWithRouteMulti Proxy q f' tag reqs baseurl opts wrap flags =
--     clientWithRouteMulti (Proxy :: Proxy sublayout) q f' tag reqs' baseurl opts wrap

--     where req' f req = req { qParams = thisPair (constDyn f) : qParams req }
--           thisPair f = (T.pack pName, QueryPartFlag f) :: (Text, QueryPart t)
--           pName      = symbolVal (Proxy :: Proxy sym)
--           reqs'      = liftA2 req' <$> flags <*> reqs


-- instance (SupportsServantReflex t m,
--           Traversable f, Applicative f) => HasClientMulti t m Raw f tag where
--   type ClientMulti t m Raw f tag = f (Dynamic t (Either Text (XhrRequest ())))
--                                  -> Event t tag
--                                  -> m (Event t (f (ReqResult tag ())))

--   clientWithRouteMulti _ _ _ _ _ _ opts wrap rawReqs triggers = do
--     let rawReqs' = sequence rawReqs :: Dynamic t (f (Either Text (XhrRequest ())))
--         rawReqs'' = attachPromptlyDynWith (\fxhr t -> Compose (t, fxhr)) rawReqs' triggers
--     wrap =<< (fmap (fmap aux . sequenceA . getCompose) <$> performSomeRequestsAsync opts rawReqs'')
--     where
--       aux (tag, Right r) = ResponseSuccess tag () r
--       aux (tag, Left  e) = RequestFailure tag e


-- instance (MimeRender ct a,
--           HasClientMulti t m sublayout f tag,
--           Reflex t,
--           Applicative f)
--       => HasClientMulti t m (ReqBody (ct ': cts) a :> sublayout) f tag where

--   type ClientMulti t m (ReqBody (ct ': cts) a :> sublayout) f tag =
--     Dynamic t (f (Either Text a)) -> ClientMulti t m sublayout f tag

--   clientWithRouteMulti Proxy q f tag reqs baseurl opts wrap bodies =
--     clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl opts wrap
--        where req'        b r = r { reqBody = bodyBytesCT (constDyn b) }
--              ctProxy         = Proxy :: Proxy ct
--              ctString        = T.pack $ show $ contentType ctProxy
--              bodyBytesCT b   = Just $ (fmap . fmap)
--                                (\b' -> (mimeRender ctProxy b', ctString))
--                                b
--              reqs'           = liftA2 req' <$> bodies <*> reqs


-- instance
--   ( KnownSymbol path,
--     HasClientMulti t m sublayout f tag,
--     Reflex t,
--     Functor f ) => HasClientMulti t m (path :> sublayout) f tag where
--   type ClientMulti t m (path :> sublayout) f tag = ClientMulti t m sublayout f tag

--   clientWithRouteMulti Proxy q f tag reqs baseurl =
--      clientWithRouteMulti (Proxy :: Proxy sublayout) q f tag
--                      (fmap (prependToPathParts (pure (Right $ T.pack p))) <$> reqs)
--                      baseurl

--     where p = symbolVal (Proxy :: Proxy path)


-- instance HasClientMulti t m api f tag => HasClientMulti t m (Vault :> api) f tag where
--   type ClientMulti t m (Vault :> api) f tag = ClientMulti t m api f tag

--   clientWithRouteMulti Proxy q f tag reqs baseurl =
--     clientWithRouteMulti (Proxy :: Proxy api) q f tag reqs baseurl


-- instance HasClientMulti t m api f tag => HasClientMulti t m (RemoteHost :> api) f tag where
--   type ClientMulti t m (RemoteHost :> api) f tag = ClientMulti t m api f tag

--   clientWithRouteMulti Proxy q f tag reqs baseurl =
--     clientWithRouteMulti (Proxy :: Proxy api) q f tag reqs baseurl


-- instance HasClientMulti t m api f tag => HasClientMulti t m (IsSecure :> api) f tag where
--   type ClientMulti t m (IsSecure :> api) f tag = ClientMulti t m api f tag

--   clientWithRouteMulti Proxy q f tag reqs baseurl =
--     clientWithRouteMulti (Proxy :: Proxy api) q f tag reqs baseurl


-- instance (HasClientMulti t m api f tag, Reflex t, Applicative f)
--       => HasClientMulti t m (BasicAuth realm usr :> api) f tag where

--   type ClientMulti t m (BasicAuth realm usr :> api) f tag = Dynamic t (f (Maybe BasicAuthData))
--                                                -> ClientMulti t m api f tag

--   clientWithRouteMulti Proxy q f tag reqs baseurl opts wrap authdatas =
--     clientWithRouteMulti (Proxy :: Proxy api) q f tag reqs' baseurl opts wrap
--       where
--         req'  a r = r { authData = Just (constDyn a) }
--         reqs' = liftA2 req' <$> authdatas <*> reqs


-- class BuildHeaderKeysTo hs where
--   buildHeaderKeysTo :: Proxy hs -> [CI.CI T.Text]

-- instance {-# OVERLAPPABLE #-} BuildHeaderKeysTo '[]
--   where buildHeaderKeysTo _ = []

-- instance {-# OVERLAPPABLE #-} (BuildHeaderKeysTo xs, KnownSymbol h)
--   => BuildHeaderKeysTo ((Header h v) ': xs) where
--   buildHeaderKeysTo _ =
--     let
--       thisKey = CI.mk $ T.pack (symbolVal (Proxy :: Proxy h))
--     in thisKey : buildHeaderKeysTo (Proxy :: Proxy xs)

-- toHeaders :: BuildHeadersTo ls => ReqResult tag a -> ReqResult tag (Headers ls a)
-- toHeaders r =
--   let hdrs = maybe []
--                    (\xhr -> fmap (\(h,v) -> (CI.map E.encodeUtf8 h, E.encodeUtf8 v))
--                      (Map.toList $ _xhrResponse_headers xhr))
--                    (response r)
--   in  ffor r $ \a -> Headers {getResponse = a ,getHeadersHList = buildHeadersTo hdrs}
