{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Description: Exampe for using wai-cors with scotty
-- Copyright: © 2015 Lars Kuhtz <lakuhtz@gmail.com>
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
module Main
( main
) where

import Network.Wai.Middleware.Cors
import Web.Scotty

main ∷ IO ()
main = scotty 8080 $ do
    middleware simpleCors
    matchAny  "/" $ text "Success"

