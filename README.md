[![Build Status](https://travis-ci.org/larskuhtz/wai-cors.svg?branch=master)](https://travis-ci.org/larskuhtz/wai-cors)

Cross-Origin Resource Sharing (CORS) For Wai
============================================

This package provides a Haskell implementation of CORS for
[WAI](http://hackage.haskell.org/package/wai)
that aims to be compliant with
[http://www.w3.org/TR/cors](http://www.w3.org/TR/cors).

Note On Security
----------------

This implementation doesn't include any server side enforcement. By complying
with the CORS standard it enables the client (i.e. the web browser) to enforce
the CORS policy. For application authors it is strongly recommended to take
into account the security considerations in section 6.3 of the
[CORS standard](http://wwww.w3.org/TR/cors). In particular the application should
check that the value of the `Origin` header matches the expectations.

Websocket connections don't support CORS and are ignored by the CORS implementation
in this package. However Websocket requests usually (at least for some
browsers) include the @Origin@ header. Applications are expected to check the
value of this header and respond with an error in case that its content doesn't
match the expectations.

Installation
------------

Assuming the availability of recent versions of
[GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/)
this package is installed via

```.bash
cabal update
cabal install wai-cors
```

Usage
-----

The function 'simpleCors' enables support of simple cross-origin requests. More
advanced CORS policies can be enabled by passing a 'CorsResourcePolicy' to the
'cors' middleware.

The file `examples/Scotty.hs` shows how to support simple cross-origin requests (as
defined in [http://www.w3.org/TR/cors](http://www.w3.org/TR/cors)) in a
[scotty](http://hackage.haskell.org/package/scotty) application.

```.haskell
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
```

The result of following curl command will include the HTTP response
header `Access-Control-Allow-Origin: *`.

```.bash
curl -i http://127.0.0.1:8888 -H 'Origin: 127.0.0.1' -v
```

Documentation for more general usage can be found in the module
[Network.Wai.Middleware.Cors](http://hackage.haskell.org/package/wai-cors/docs/Network-Wai-Middleware-Cors.html).

Test
----

In order to run the automated test suite [PhantomJS](http://phantomjs.org/) (at
least version 2.0) must be installed in the system.

```.bash
cabal install --only-dependencies --enable-tests
cabal test --show-details=streaming
```

If [PhantomJS](http://phantomjs.org/) is not available the tests can be
exectued manually in a modern web-browser as follows.

Start the server application:

```.bash
cd test
ghc -main-is Server Server.hs
./Server
```

Open the file `test/index.html` in a modern web-browser. On page load a Javascript
script is exectued that runs the test suite and prints the result on the page.

