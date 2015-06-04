{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Server
-- Description: Test HTTP server for wai-cors
-- Copyright: © 2015 Lars Kuhtz <lakuhtz@gmail.com>.
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
module Server
( main
) where

#ifndef MIN_VERSION_wai
#define MIN_VERSION_wai(a,b,c) 1
#endif

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
main = withSocketsDo $
    WARP.run 8080 $ simpleCors waiapp

waiapp ∷ WAI.Application
waiapp = WS.websocketsOr WS.defaultConnectionOptions wsserver $ \_ → app
  where
#if MIN_VERSION_wai(2,0,0)
    app respond = respond $ WAI.responseLBS HTTP.status200 [] "Success"
#else
    app = WAI.responseLBS HTTP.status200 [] "Success"
#endif

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

