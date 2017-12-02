{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Wai
-- Description: Example for using wai-cors with a plain wai app
-- Copyright: Copyright © 2017 Lars Kuhtz <lakuhtz@gmail.com>.
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
module Wai
( main
) where

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp

main ∷ IO ()
main = run 8080 $ simpleCors $ echo

echo ∷ Application
echo req respond =
    respond . responseLBS ok200 [] =<< lazyRequestBody req

