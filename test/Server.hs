{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Server
-- Description: Test HTTP server for wai-cors
-- Copyright: © 2015-2018 Lars Kuhtz <lakuhtz@gmail.com>.
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
module Server
( main
) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Network.Socket (withSocketsDo)
import Network.Wai.Middleware.Cors
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as WARP
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS

import qualified Data.Text as T

main ∷ IO ()
main = withSocketsDo . WARP.run 8080 $ server

-- -------------------------------------------------------------------------- --
-- Server application

server ∷ WAI.Application
server = cors corsPolicy $ \request →
    case WAI.pathInfo request of
        "cors":_ → corsapp request
        _ → testapp
  where
    testapp respond = respond $ WAI.responseFile HTTP.status200 [] "index.html" Nothing
    corsapp = WS.websocketsOr WS.defaultConnectionOptions wsserver $ \_ respond →
        respond $ WAI.responseLBS HTTP.status200 [] "Success"

-- -------------------------------------------------------------------------- --
-- CORS Policy

corsPolicy ∷ WAI.Request → Maybe CorsResourcePolicy
corsPolicy request = case WAI.pathInfo request of
    "cors" : "non-simple":_ → Just nonSimplePolicy
    "cors" : "simple":_ → Just simpleCorsResourcePolicy
    _ → Nothing

-- -------------------------------------------------------------------------- --
-- Websockets Server

wsserver ∷ WS.ServerApp
wsserver pc = do
    c ← WS.acceptRequest pc
    forever (go c) `catch` \case
        WS.CloseRequest _code _msg → WS.sendClose c ("closed" ∷ T.Text)
        e → throwIO e
  where
    go c = do
        msg ← WS.receiveDataMessage c
        forkIO $ WS.sendDataMessage c msg

-- -------------------------------------------------------------------------- --
-- Non Simple Policy

-- | Perform the following tests the following with this policy:
--
-- * @Variy: Origin@ header is set on responses
-- * @X-cors-test@ header is accepted
-- * @X-cors-test@ header is exposed on response
-- * @Access-Control-Allow-Origin@ header is set on responses to the request host
-- * @DELETE@ requests are not allowed
-- * @PUT@ requests are allowed
-- * Requests that don't include an @Origin@ header result in 400 responses
--   (it's not clear how to test this with a browser client)
--
-- Note that Chrome sends @Origin: null@ when loaded from a "file://..." URL,
-- PhantomJS sends "file://".
--
nonSimplePolicy ∷ CorsResourcePolicy
nonSimplePolicy = CorsResourcePolicy
    { corsOrigins = Just (["http://localhost:8080", "null", "file://"], False)
    , corsMethods = ["PUT"]
    , corsRequestHeaders = ["X-cors-test"]
    , corsExposedHeaders = Just ["X-cors-test", "Vary"]
    , corsMaxAge = Nothing
    , corsVaryOrigin = True
    , corsRequireOrigin = True
    , corsIgnoreFailures = False
    }

