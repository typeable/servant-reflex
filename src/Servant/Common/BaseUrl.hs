{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Common.BaseUrl (
  -- * types
    BaseUrl (..)
  , Scheme (..)
  -- * functions
  , baseUrlWidget
  , showBaseUrl

  -- * constraints
  , SupportsServantReflex
) where

import           Control.Monad (join)
-- import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Fix (MonadFix)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Language.Javascript.JSaddle.Monad (MonadJSM)
import           Reflex
import           Reflex.Dom.Core
import           Text.Read

#if !MIN_VERSION_reflex_dom_core(0,7,0)
type SupportsServantReflex t m = (Reflex t, TriggerEvent t m, PerformEvent t m, HasWebView (Performable m), MonadJSM (Performable m))
#else
type SupportsServantReflex t m = (Reflex t, TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
#endif

-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving (Show, Read, Eq, Ord, Generic)

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseFullUrl Scheme Text Int Text
             | BasePath Text
  deriving (Ord, Read, Show, Generic)


instance Eq BaseUrl where
    BasePath s == BasePath s' = s == s'
    BaseFullUrl a b c path == BaseFullUrl a' b' c' path'
        = a == a' && b == b' && c == c' && s path == s path'
        where s x = if T.isPrefixOf "/" x then T.tail x else x
    _ == _ = False

showBaseUrl :: BaseUrl -> Text
showBaseUrl (BasePath s) = s
showBaseUrl (BaseFullUrl urlscheme host port path) =
  schemeString <> "//" <> host <> (portString </> path)
    where
      a </> b = if "/" `T.isPrefixOf` b || T.null b then a <> b else a <> "/" <> b
      schemeString = case urlscheme of
        Http  -> "http:"
        Https -> "https:"
      portString = case (urlscheme, port) of
        (Http, 80) -> ""
        (Https, 443) -> ""
        _ -> ":" <> T.pack (show port)

baseUrlWidget :: forall t m .(SupportsServantReflex t m,
                              DomBuilderSpace m ~ GhcjsDomSpace,
                              MonadFix m,
                              PostBuild t m,
                              MonadHold t m,
                              DomBuilder t m)
              => m (Dynamic t BaseUrl)
baseUrlWidget = elClass "div" "base-url" $ do
  urlWidget <- dropdown (0 :: Int) (constDyn $ 0 =: "BasePath" <> 1 =: "BaseUrlFull") def
  let bUrlWidget = ffor (value urlWidget) $ \i -> case i of
        0 -> pathWidget
        1 -> fullUrlWidget
        _ -> error "Surprising value"
  join <$> widgetHold pathWidget (updated bUrlWidget)
  where pathWidget :: m (Dynamic t BaseUrl)
        pathWidget = do
          text "Url base path"
          t <- inputElement def
          return $ BasePath <$> value t
        fullUrlWidget :: m (Dynamic t BaseUrl)
        fullUrlWidget = do
          schm <- dropdown Https (constDyn $ Https =: "https" <> Http =: "http") def
          srv  <- inputElement def
          text ":"
          prt  <- inputElement def
          port :: Dynamic t Int <- holdDyn 80 (fmapMaybe (readMaybe . T.unpack) $ updated (value prt))
          path <- inputElement def
          return $ BaseFullUrl <$> value schm <*> value srv <*> port <*> value path
