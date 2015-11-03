{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Main
-- Description: Websocket test for wai-cors
-- Copyright: Copyright © 2015 Lars Kuhtz <lakuhtz@gmail.com>.
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
module Main
( main
) where

import Control.Exception
import Control.Monad

import qualified Network.HTTP.Types as HTTP
import Network.Socket (withSocketsDo)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as WARP
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS

import qualified Data.Text as T

main ∷ IO ()
main = withSocketsDo $
    forkIO $ WARP.run 8080 waiapp

-- -------------------------------------------------------------------------- --
-- Client

{-
client :: WS.ClientApp
client = WS.runClient "127.0.0.1" 8080 "/" $ \c -> do
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData c
        T.putStrLn msg
-}

-- -------------------------------------------------------------------------- --
-- Server

waiapp ∷ WAI.Application
waiapp = WS.websocketsOr WS.defaultConnectionOptions server $ \_req respond -> do
    -- respond $ WAI.responseLBS HTTP.status500 [] "expected a websockets request"
    respond $ WAI.responseLBS HTTP.status200 [] "Success"

server :: WS.ServerApp
server pc = do
    c <- WS.acceptRequest pc
    WS.sendTextData c ("websocket test message" :: T.Text)
    WS.sendClose c ("closed" :: T.Text)
    void (WS.receiveDataMessage c) `catch` \case
        WS.CloseRequest _code _msg -> return ()
        e -> throwIO e
