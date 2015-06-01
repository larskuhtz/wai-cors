-- ------------------------------------------------------ --
-- Copyright © 2015 Lars Kuhtz <lakuhtz@gmail.com>
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}

-- | An implemenation of Cross-Origin resource sharing (CORS) for WAI that
-- aims to be compliant with <http://www.w3.org/TR/cors>.
--
-- The function 'simpleCors' enables support of simple cross-origin requests. More
-- advanced CORS policies can be enabled by passing a 'CorsResourcePolicy' to the
-- 'cors' middleware.
--
-- = Note On Security
--
-- This implementation doens't include any server side enforcement. By
-- complying with the CORS standard it enables the client (i.e. the web
-- browser) to enforce the CORS policy. For application authors it is strongly
-- recommended to take into account the security considerations in section 6.3
-- of <http://wwww.w3.org/TR/cors>. In particular the application should check
-- that the value of the @Origin@ header matches it's expectations.
--
-- = Websockets
--
-- Websocket connections don't support CORS and are ignored by this CORS
-- implementation. However Websocket requests usually (at least for some
-- browsers) include the @Origin@ header. Applications are expected to check
-- the value of this header and respond with an error in case that its content
-- doesn't match the expectations.
--
-- = Example
--
-- The following is an example how to enable support for simple cross-origin requests
-- for a <http://hackage.haskell.org/package/scotty scotty> application.
--
-- > {-# LANGUAGE UnicodeSyntax #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main
-- > ( main
-- > ) where
-- >
-- > import Network.Wai.Middleware.Cors
-- > import Web.Scotty
-- >
-- > main ∷ IO ()
-- > main = scotty 8080 $ do
-- >     middleware simpleCors
-- >     matchAny  "/" $ text "Success"
--
-- The result of following curl command will include the HTTP response header
-- @Access-Control-Allow-Origin: *@.
--
-- > curl -i http://127.0.0.1:8888 -H 'Origin: 127.0.0.1' -v
--
module Network.Wai.Middleware.Cors
( Origin
, CorsResourcePolicy(..)
, simpleCorsResourcePolicy
, cors
, simpleCors

-- * Utils
, isSimple
, simpleResponseHeaders
, simpleHeaders
, simpleContentTypes
, simpleMethods
) where

#ifndef MIN_VESION_base
#define MIN_VESION_base(x,y,z) 1
#endif

#ifndef MIN_VESION_wai
#define MIN_VESION_wai(x,y,z) 1
#endif

#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
#if ! MIN_VERSION_wai(2,0,0)
import Control.Monad.Trans.Resource
#endif

import qualified Data.Attoparsec.ByteString as AttoParsec
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.CaseInsensitive as CI
import qualified Data.CharSet as CS
import Data.List (intersect, (\\), union)
import Data.Maybe (catMaybes)
import Data.Monoid.Unicode
import Data.String

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI

import Prelude.Unicode

import qualified Text.Parser.Char as P
import qualified Text.Parser.Combinators as P

#if MIN_VERSION_wai(2,0,0)
type ReqMonad = IO
#else
type ReqMonad = ResourceT IO
#endif

-- | Origins are expected to be formated as described in RFC 6454 (section
-- 6.2). In particular the string @*@ is not a valid origin (but the string
-- @null@ is).
--
type Origin = B8.ByteString

data CorsResourcePolicy = CorsResourcePolicy
    {

    -- | HTTP origins that are allowed in CORS requests.
    --
    -- A value of 'Nothing' indicates unrestricted cross-origin sharing and
    -- results in @*@ as value for the @Access-Control-Allow-Origin@ HTTP
    -- response header.
    --
    -- A value other than 'Nothing' is a tuple that consists of a list of
    -- origins each with a Boolean flag that indicates if credentials are used
    -- to access the resource via CORS.
    --
    -- Origins must be formated as described in RFC6454 (section 6.2). In
    -- particular the string @*@ is not a valid origin (but the string @null@
    -- is).
    --
       corsOrigins ∷ !(Maybe ([Origin], Bool))

    -- | HTTP methods that are allowed in CORS requests.
    --
    , corsMethods ∷ ![HTTP.Method]

    -- | Field names of HTTP request headers that are allowed in CORS requests.
    -- Header names that are included in 'simpleHeaders', except for
    -- @content-type@, are implicitely included an thus optional in this list.
    --
    , corsRequestHeaders ∷ ![HTTP.HeaderName]

    -- | Field names of HTTP headers that are exposed on the client.
    --
    , corsExposedHeaders ∷ !(Maybe [HTTP.HeaderName])

    -- | Number of seconds that the response may be cached by the client.
    --
    , corsMaxAge ∷ !(Maybe Int)

    -- | If the resource is shared by multiple origins but
    -- @Access-Control-Allow-Origin@ is not set to @*@ this may be set to
    -- 'True'.
    --
    , corsVaryOrigin ∷ !Bool

    -- | If this is 'True' and the request does not include an @Origin@ header
    -- the response has HTTP status 400 (bad request) and the body contains
    -- a short error message.
    --
    -- If this is 'False' and the request does not include an @Origin@ header
    -- the request is passed on unchanged to the application.
    --
    -- @since 0.2
    , corsRequireOrigin ∷ !Bool

    -- | In the case that
    --
    -- * the request contains an @Origin@ header and
    --
    -- * the client does not conform with the CORS protocol
    --   (/request is out of scope/)
    --
    -- then
    --
    -- * the request is passed on unchanged to the application if this field is
    --   'True' or
    --
    -- * an response with HTTP status 400 (bad request) and short
    --   error message is returned if this field is 'False'.
    --
    -- @since 0.2
    --
    , corsIgnoreFailures ∷ !Bool
    }
    deriving (Show,Read,Eq,Ord)

-- | A 'CorsResourcePolicy' that supports /simple cross-origin requests/ as defined
-- in <http://www.w3.org/TR/cors/>.
--
-- * The HTTP header @Access-Control-Allow-Origin@ is set to @*@.
--
-- * Request methods are constraint to /simple methods/ (@GET@, @HEAD@, @POST@).
--
-- * Request headers are constraint to /simple request headers/
--   (@Accept@, @Accept-Language@, @Content-Language@, @Content-Type@).
--
-- * If the request is a @POST@ request the content type is constraint to
--    /simple content types/
--    (@application/x-www-form-urlencoded@, @multipart/form-data@, @text/plain@),
--
-- * Only /simple response headers/ may be exposed on the client
--   (@Cache-Control@, @Content-Language@, @Content-Type@, @Expires@, @Last-Modified@,  @Pragma@)
--
-- * The @Vary-Origin@ header is left unchanged (possibly unset).
--
-- * If the request doesn't include an @Origin@ header the request is passed unchanged to
--   the application.
--
-- * If the request includes an @Origin@ header but does not conform to the CORS
--   protocol (/request is out of scope/) an response with HTTP status 400 (bad request)
--   and a short error message is returned.
--
-- For /simple cross-origin requests/ a preflight request is not required. However, if
-- the client chooses to make a preflight request it is answered in accordance with
-- the policy for /simple cross-origin requests/.
--
simpleCorsResourcePolicy ∷ CorsResourcePolicy
simpleCorsResourcePolicy = CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = simpleMethods
    , corsRequestHeaders = []
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

-- | A Cross-Origin resource sharing (CORS) middleware.
--
-- The middleware is given a function that serves as a pattern to decide
-- whether a requested resource is available for CORS. If the match fails with
-- 'Nothing' the request is passed unmodified to the inner application.
--
-- The current version of this module does only aim at compliance with the CORS
-- protocol as specified in <http://www.w3.org/TR/cors/>. In accordance with
-- that standard the role of the server side is to support the client to
-- enforce CORS restrictions. This module does not implement any enforcement of
-- authorization policies that are possibly implied by the
-- 'CorsResourcePolicy'. It is up to the inner WAI application to enforce such
-- policy and make sure that it is in accordance with the configuration of the
-- 'cors' middleware.
--
-- Matches are done as follows: @*@ matches every origin. For all other cases a
-- match succeeds if and only if the ASCII serializations (as described in
-- RCF6454 section 6.2) are equal.
--
-- The OPTIONS method may return options for resources that are not actually
-- available. In particular for preflight requests the implementation returns
-- for the HTTP response headers @Access-Control-Allow-Headers@ and
-- @Access-Control-Allow-Methods@ all values specified in the
-- 'CorsResourcePolicy' together with the respective values for simple requests
-- (except @content-type@). This does not imply that the application actually
-- supports the respective values are for the requested resource. Thus,
-- depending on the application, an actual request may still fail with 404 even
-- if the preflight request /supported/ the usage of the HTTP method with CORS.
--
-- The implementation does not distinguish between simple requests and requests
-- that require preflight. The client is free to omit a preflight request or do
-- a preflight request in cases when it wouldn't be required.
--
-- For application authors it is strongly recommended to take into account the
-- security considerations in section 6.3 of <http://wwww.w3.org/TR/cors>.
--
-- /TODO/
--
-- * We may consider adding optional enforcment aspects to this module: we may
--   check if a request respects our origin restrictions and we may check that a
--   CORS request respects the restrictions that we publish in the preflight
--   responses.
--
-- * Even though slightly out of scope we may (optionally) check if
--   host header matches the actual host of the resource, since clients
--   using CORS may expect this, since this check is recommended in
--   <http://www.w3.org/TR/cors>.
--
-- * We may consider integrating CORS policy handling more closely with the
--   handling of the source, for instance by integrating with 'ActionM' from
--   scotty.
--
cors
    ∷ (WAI.Request → Maybe CorsResourcePolicy) -- ^ A value of 'Nothing' indicates that the resource is not available for CORS
    → WAI.Middleware
#if MIN_VERSION_wai(3,0,0)
cors policyPattern app r respond
#else
cors policyPattern app r
#endif
    -- We don't handle websockets, even if they include an @Origin@ header
    | isWebSocketsReq r = runApp

    | Just policy ← policyPattern r = case hdrOrigin of

        -- No origin header: requect request
        Nothing → if corsRequireOrigin policy
            then res $ corsFailure "Origin header is missing"
            else runApp

        -- Origin header: apply CORS policy to request
        Just origin → applyCorsPolicy policy origin

    | otherwise = runApp
  where

#if MIN_VERSION_wai(3,0,0)
    res = respond
    runApp = app r respond
#else
    res = return
    runApp = app r
#endif

    -- Lookup the HTTP origin request header
    --
    hdrOrigin = lookup "origin" (WAI.requestHeaders r)

    -- Process a CORS request
    --
    applyCorsPolicy policy origin = do

        -- The error continuation
        let err e = if corsIgnoreFailures policy
            then runApp
            else res $ corsFailure (B8.pack e)

        -- Match request origin with corsOrigins from policy
        let respOrigin = case corsOrigins policy of
                Nothing → return Nothing
                Just (originList, withCreds) → if origin `elem` originList
                    then Right $ Just (origin, withCreds)
                    else Left $ "Unsupported origin: " ⊕ B8.unpack origin

        case respOrigin of
            Left e → err e
            Right respOrigin → do

                -- Determine headers that are common to actuall responses and preflight responses
                let ch = commonCorsHeaders respOrigin (corsVaryOrigin policy)

                case WAI.requestMethod r of

                    -- Preflight CORS request
                    "OPTIONS" → runExceptT (preflightHeaders policy) >>= \case
                        Left e → err e
                        Right headers → res $ WAI.responseLBS HTTP.ok200 (ch ⊕ headers) ""

                    -- Actual CORS request
#if MIN_VERSION_wai(3,0,0)
                    _ → addHeaders (ch ⊕ respCorsHeaders policy) app r respond
#else
                    _ → addHeaders (ch ⊕ respCorsHeaders policy) app r
#endif

    -- Compute HTTP response headers for a preflight request
    --
    preflightHeaders ∷ (Functor μ, Monad μ) ⇒ CorsResourcePolicy → ExceptT String μ HTTP.ResponseHeaders
    preflightHeaders policy = concat <$> sequence
        [ hdrReqMethod policy
        , hdrRequestHeader policy
        , hdrMaxAge policy
        ]

    hdrMaxAge ∷ Monad μ ⇒ CorsResourcePolicy → ExceptT String μ HTTP.ResponseHeaders
    hdrMaxAge policy = case corsMaxAge policy of
        Nothing → return []
        Just secs → return [("Access-Control-Max-Age", sshow secs)]

    hdrReqMethod ∷ Monad μ ⇒ CorsResourcePolicy → ExceptT String μ HTTP.ResponseHeaders
    hdrReqMethod policy = case lookup "Access-Control-Request-Method" (WAI.requestHeaders r) of
        Nothing → throwError "Access-Control-Request-Method header is missing in CORS preflight request"
        Just x → if  x `elem` supportedMethods
            then return [("Access-Control-Allow-Methods", hdrL supportedMethods)]
            else throwError
                $ "Method requested in Access-Control-Request-Method of CORS request is not supported; requested: "
                ⊕ B8.unpack x
                ⊕ "; supported are "
                ⊕ B8.unpack (hdrL supportedMethods)
                ⊕ "."
      where
         supportedMethods = corsMethods policy `union` simpleMethods

    hdrRequestHeader ∷ Monad μ ⇒ CorsResourcePolicy → ExceptT String μ HTTP.ResponseHeaders
    hdrRequestHeader policy = case lookup "Access-Control-Request-Headers" (WAI.requestHeaders r) of
        Nothing → return []
        Just hdrsBytes → do
            hdrs ← either throwError return $ AttoParsec.parseOnly httpHeaderNameListParser hdrsBytes
            if hdrs `isSubsetOf` supportedHeaders
                then return [("Access-Control-Allow-Headers", hdrLI supportedHeaders)]
                else throwError
                    $ "HTTP header requested in Access-Control-Request-Headers of CORS request is not supported; requested: "
                    ⊕ B8.unpack (hdrLI hdrs)
                    ⊕ "; supported are "
                    ⊕ B8.unpack (hdrLI supportedHeaders)
                    ⊕ "."
      where
        supportedHeaders = corsRequestHeaders policy `union` simpleHeadersWithoutContentType

    simpleHeadersWithoutContentType = simpleHeaders \\ ["content-type"]

    -- HTTP response headers that are common to normal and preflight CORS responses
    --
    commonCorsHeaders ∷ Maybe (Origin, Bool) → Bool → HTTP.ResponseHeaders
    commonCorsHeaders Nothing True = [("Access-Control-Allow-Origin", "*"), ("Vary", "Origin")]
    commonCorsHeaders Nothing False = [("Access-Control-Allow-Origin", "*")]
    commonCorsHeaders (Just (o, False)) _ = [("Access-Control-Allow-Origin", o)]
    commonCorsHeaders (Just (o, True)) _  = [("Access-Control-Allow-Origin", o), ("Access-Control-Allow-Credentials", "true")]

    -- HTTP response headers that are only used with normal CORS responses
    --
    respCorsHeaders ∷ CorsResourcePolicy → HTTP.ResponseHeaders
    respCorsHeaders policy = catMaybes
        [ fmap (\x → ("Access-Control-Expose-Headers", hdrLI x)) (corsExposedHeaders policy)
        ]

-- | A CORS middleware that supports simple cross-origin requests for all
-- resources.
--
-- This middleware does not check if the resource corresponds to the
-- restrictions for simple requests. This is in accordance with
-- <http://www.w3.org/TR/cors/>. The client (user-agent) is supposed to
-- enforcement CORS policy. The role of the server is to provide the client
-- with the respective policy constraints.
--
-- It is out of the scope of the this module if the server chooses to
-- enforce rules on its resources in relation to CORS policy itself.
--
simpleCors ∷ WAI.Middleware
simpleCors = cors (const $ Just simpleCorsResourcePolicy)

-- -------------------------------------------------------------------------- --
-- Definition from Standards

-- | Simple HTTP response headers as defined in <http://www.w3.org/TR/cors/>
--
simpleResponseHeaders ∷ [HTTP.HeaderName]
simpleResponseHeaders =
    [ "Cache-Control"
    , "Content-Language"
    , "Content-Type"
    , "Expires"
    , "Last-Modified"
    , "Pragma"
    ]

simpleHeaders ∷ [HTTP.HeaderName]
simpleHeaders =
    [ "Accept"
    , "Accept-Language"
    , "Content-Language"
    , "Content-Type"
    ]

simpleContentTypes ∷ [CI.CI B8.ByteString]
simpleContentTypes =
    [ "application/x-www-form-urlencoded"
    , "multipart/form-data"
    , "text/plain"
    ]


-- | Simple HTTP methods as defined in <http://www.w3.org/TR/cors/>
--
simpleMethods ∷ [HTTP.Method]
simpleMethods =
    [ "GET"
    , "HEAD"
    , "POST"
    ]

isSimple ∷ HTTP.Method → HTTP.RequestHeaders → Bool
isSimple method headers
    = method `elem` simpleMethods
    ∧ map fst headers `isSubsetOf` simpleHeaders
    ∧ case (method, lookup "content-type" headers) of
        ("POST", Just x) → CI.mk x `elem` simpleContentTypes
        _ → True

-- | Valid characters for HTTP header names according to RFC2616 (section 4.2)
--
httpHeaderNameCharSet ∷ CS.CharSet
httpHeaderNameCharSet = CS.range (toEnum 33) (toEnum 126) CS.\\ CS.fromList "()<>@,;:\\\"/[]?={}"

httpHeaderNameParser ∷ P.CharParsing μ ⇒ μ HTTP.HeaderName
httpHeaderNameParser = fromString <$> P.some (P.oneOfSet httpHeaderNameCharSet) P.<?> "HTTP Header Name"

-- -------------------------------------------------------------------------- --
-- Generic Tools

httpHeaderNameListParser ∷ P.CharParsing μ ⇒ μ [HTTP.HeaderName]
httpHeaderNameListParser = P.spaces *> P.sepBy (httpHeaderNameParser <* P.spaces) (P.char ',') <* P.spaces

sshow ∷ (IsString α, Show β) ⇒ β → α
sshow = fromString ∘ show

isSubsetOf ∷ Eq α ⇒ [α] → [α] → Bool
isSubsetOf l1 l2 = intersect l1 l2 ≡ l1

-- | Add HTTP headers to a WAI response
--
#if MIN_VERSION_wai(3,0,0)
addHeaders ∷ HTTP.ResponseHeaders → WAI.Middleware
addHeaders hdrs app req respond = app req $ \response → do
    let (st, headers, streamHandle) = WAI.responseToStream response
    streamHandle $ \streamBody →
        respond $ WAI.responseStream st (headers ⊕ hdrs) streamBody

#elif MIN_VERSION_wai(2,0,0)

addHeaders ∷ HTTP.ResponseHeaders → WAI.Middleware
addHeaders hdrs app req = do
    (st, headers, src) ← WAI.responseToSource <$> app req
    WAI.responseSource st (headers ⊕ hdrs) <$> src return

#else

addHeaders ∷ HTTP.ResponseHeaders → WAI.Middleware
addHeaders hdrs app req = do
    (st, headers, src) ← WAI.responseSource <$> app req
    return $ WAI.ResponseSource st (headers ⊕ hdrs) src
#endif

-- | Format a list of 'HTTP.HeaderName's such that it can be used as
-- an HTTP header value
--
hdrLI ∷ [HTTP.HeaderName] → B8.ByteString
hdrLI l = B8.intercalate ", " (map CI.original l)

-- | Format a list of 'B8.ByteString's such that it can be used as
-- an HTTP header value
--
hdrL ∷ [B8.ByteString] → B8.ByteString
hdrL l = B8.intercalate ", " l

corsFailure
    ∷ B8.ByteString -- ^ body
    → WAI.Response
corsFailure msg = WAI.responseLBS HTTP.status400 [("Content-Type", "text/html; charset-utf-8")] (LB8.fromStrict msg)

-- Copied from the [wai-websocket package](https://github.com/yesodweb/wai/blob/master/wai-websockets/Network/Wai/Handler/WebSockets.hs#L21)
--
isWebSocketsReq
    ∷ WAI.Request
    → Bool
isWebSocketsReq req =
    fmap CI.mk (lookup "upgrade" $ WAI.requestHeaders req) == Just "websocket"

