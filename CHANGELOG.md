0.2.5
=====

*   Support GHC-8.0.1.

*   Removed dependencies on parsers package.

0.2.4
=====

*   Fix [bug #1](https://github.com/larskuhtz/wai-cors/issues/1).
    Response header `Vary: Origin` is now included when `corsVaryOrigin`
    is `True` and `corsOrigins` does not equal `Nothing`.

0.2.3
=====

*   Added a test-suite to the package that uses PhantomJS to simulate a
    browser client.

*   Pass on websocket requests unchanged to the application. Add documentation
    that reminds the application author to check the `Origin` header for
    websocket requests.

*   Move development source repository from https://github.com/alephcloud/wai-cors
    to https://github.com/larskuhtz/wai-cors.

0.2.2
=====

*   Support GHC-7.10/base-4.8 without compiler warnings.

*   Drop dependency on errors package.

0.2.1
=====

*   Fix [bug #8](https://github.com/alephcloud/wai-cors/issues/8).
    Accept empty list as value for `Access-Control-Request-Headers`.

0.2
===

This version may break existing code by changing the type of
`CorsResourcePolicy`.

This version changes the behavior of `simpleCorsResourcePolicy`: Before
it was a failure when the request didn't contain an @Origin@ header.
With this version the request is passed unchanged to the application.
If an failure occurs during CORS processing the response has HTTP status
400 (bad request) and contains a short error messages. This behavior
can be changed with the new settings `corsRequireOrigin` and
`corsIgnorefailure`.

*   Remove setting `corsVerboseResponse` from `CorsResourcePolicy`.

*   Add new settings `corsRequireOrigin` and `corsIgnoreFailure` to
    `CorsResourcePolicy`.

0.1.4
=====

*   Support wai-3

0.1.3
=====

*   Improved documentation

0.1.2
=====

*   Export type synonym `Origin`

*   New easy-to-use middleware function `simpleCors` that supports just
    simple cross-origin requests

*   The new value `simpleCorseResourcePolicy` is a `CorseResourcePolicy`
    for simple cross-origin requests.

*   Documentation has been slightly improved

*   Some code for testing `simpleCors` from a browser

0.1.1
=====

*   Drop redundant dependencies on *lens* and *ghc-prim*

*   Fix typo in HTTP header field name `Access-Control-Allow-Credentials`

0.1.0
=====

*   Initial version

