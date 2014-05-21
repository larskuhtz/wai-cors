-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import Network.Wai.Middleware.Cors
import Web.Scotty

main ∷ IO ()
main = scotty 8080 $ do
    middleware simpleCors
    matchAny  "/" $ text "Success"

