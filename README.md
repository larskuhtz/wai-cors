Cross-Origin Resource Sharing (CORS) For Wai
============================================

This package provides a Haskell implemenation of CORS for
[WAI](http://hackage.haskell.org/package/wai)
that aims to be compliant with
[http://www.w3.org/TR/cors](http://www.w3.org/TR/cors).

TEST
====

Currently there is only basic support to test simple cross-origin
request from a browser.

Start server:

~~~{.bash}
cd test
runHaskell server.hs
~~~

Open the file `test/index.html` in a modern web-browser in order to
run some simple tests.
