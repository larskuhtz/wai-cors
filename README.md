[![Build Status](https://travis-ci.org/alephcloud/wai-cors.svg?branch=master)](https://travis-ci.org/alephcloud/wai-cors)

Cross-Origin Resource Sharing (CORS) For Wai
============================================

This package provides a Haskell implemenation of CORS for
[WAI](http://hackage.haskell.org/package/wai)
that aims to be compliant with
[http://www.w3.org/TR/cors](http://www.w3.org/TR/cors).

Usage
-----

The file `test/server.hs` shows how to support simple cross-origin requests (as
defined in [http://www.w3.org/TR/cors](http://www.w3.org/TR/cors)) in a
[scotty](http://hackage.haskell.org/package/scotty) application.

~~~{.haskell}
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
~~~

The result of following curl command will include the HTTP response 
header `Access-Control-Allow-Origin: *`.

~~~{.bash}
curl -i http://127.0.0.1:8888 -H 'Origin: 127.0.0.1' -v
~~~

Documentation for more general usage can be found in the module
[Network.Wai.Middleware.Cors](http://hackage.haskell.org/package/wai-cors/docs/Network-Wai-Middleware-Cors.html).

TEST
----

Currently there is only basic support to test simple cross-origin
request from a browser.

Start server:

~~~{.bash}
cd test
runHaskell server.hs
~~~

Open the file `test/index.html` in a modern web-browser in order to run some
simple tests.
