-- ------------------------------------------------------ --
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

-- | An implemenation of CORS for WAI that aims to be compliant with
-- <http://www.w3.org/TR/cors>.
--
module Network.Wai.Middleware.Cors
( CorsResourcePolicy(..)
, cors

-- * Utils
, isSimple
, simpleResponseHeaders
, simpleHeaders
, simpleContentTypes
, simpleMethods
) where

import Control.Applicative
import Control.Error
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource

import qualified Data.Attoparsec as AttoParsec
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.CaseInsensitive as CI
import qualified Data.CharSet as CS
import Data.List (intersect, (\\), union)
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
    -- A value of 'Nothing' indicates in unrestricted cross-origin sharing and
    -- result in @*@ as value for the @Access-Control-Allow-Origin@ HTTP
    -- response header.
    --
    -- If not 'Nothing' the value is a tuple that consits of a list of origins
    -- and a Boolean flag that indicates if credentials are used to access the
    -- resource via CORS.
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
    -- @content-type@, are implicitely included an thus optionsal
    --
    , corsRequestHeaders ∷ ![HTTP.HeaderName]

    -- | Field names of HTTP headers that are exposed on the client.
    --
    , corsExposedHeaders ∷ !(Maybe [HTTP.HeaderName])

    -- | Number of seconds that the response may be cached by the client.
    --
    , corsMaxAge ∷ !(Maybe Int)

    -- | If the resource if shared by multiple origins but
    -- @Access-Control-Allow-Origin@ is not set to @*@ this may be set to
    -- 'True'.
    --
    , corsVaryOrigin ∷ !Bool

    -- | If 'True' verbose responses with HTTP status 400 (bad request) are
    -- returned in case of an failure. Otherwise status 200 is used along with
    -- an empty body.
    --
    , corsVerboseResponse ∷ !Bool
    }
    deriving (Show,Read,Eq,Ord)

-- | A basic CORS middleware.
--
-- The middleware is given a function that serves as a pattern to decide
-- whether a requested resource is available for CORS. If the match fails with
-- 'Nothing' the request is passed unmodified to the inner application.
--
-- The current version of this module does only aim at compliance with the CORS
-- protocol as specified in <http://www.w3.org/TR/cors/>. It does not implement
-- any enforcement of authorization policies that are possibly implied by the
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
-- (except @content-type@). This does not imply that the respective values are
-- actually supported for the Resource by the application. Thus, depending on
-- the application, an actual request may still fail with 404 even if the
-- preflight request /supported/ the usage of the HTTP method with CORS.
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
-- * We may consider adding enforcment aspects to this module:
--
--     * we may check if a request respects our origin restrictions and
--     * we may check that a CORS request respects the restrictions that we
--       publish in the preflight responses.
--
-- * Even though slightly out of scope we may (optionally) check if
--   host header matches the actual host of the resource, since clients
--   using CORS may expect this. This is check is recommended in
--   <http://www.w3.org/TR/cors>.
--
-- * We may consider integrating CORS policy handling more
--   closely with the handling of the source, for instance
--   by integrating with 'ActionM' from scotty.
--
cors
    ∷ (WAI.Request → Maybe CorsResourcePolicy) -- ^ A value of 'Nothing' indicates that the resource is not available for CORS
    → WAI.Middleware
cors policyPattern app r
    | Just policy ← policyPattern r = case hdrOrigin of

        -- No origin header: requect request
        Nothing → return $ corsFailure (corsVerboseResponse policy) "Origin header is missing"

        -- Origin header: apply CORS policy to request
        Just origin → runEitherT (applyCorsPolicy policy origin) >>= \case
                Left e → return $ corsFailure (corsVerboseResponse policy) (B8.pack e)
                Right response → return response

    | otherwise = app r

  where

    -- Lookup the HTTP origin request header
    --
    hdrOrigin = lookup "origin" (WAI.requestHeaders r)

    -- Process a CORS request
    --
    applyCorsPolicy
        ∷ CorsResourcePolicy
        → Origin
        → EitherT String ReqMonad WAI.Response
    applyCorsPolicy policy origin = do

        -- Match request origin with corsOrigins from policy
        respOrigin ← case corsOrigins policy of
            Nothing → return Nothing
            Just (originList, withCreds) → if origin `elem` originList
                then return $ Just (origin, withCreds)
                else left $ "Unsupported origin: " ⊕ B8.unpack origin

        -- Determine headers that are common to actuall responses and preflight responses
        let ch = commonCorsHeaders respOrigin (corsVaryOrigin policy)

        case WAI.requestMethod r of

            -- Preflight CORS request
            "OPTIONS" → do
                headers ← (⊕) <$> pure ch <*> preflightHeaders policy
                return $ WAI.responseLBS HTTP.ok200 headers ""

            -- Actual CORS request
            _ → lift $ app r >>= addHeaders (ch ⊕ respCorsHeaders policy)

    -- Compute HTTP response headers for a preflight request
    --
    preflightHeaders ∷ Monad μ ⇒ CorsResourcePolicy → EitherT String μ HTTP.ResponseHeaders
    preflightHeaders policy = concat <$> sequence
        [ hdrReqMethod policy
        , hdrRequestHeader policy
        , hdrMaxAge policy
        ]

    hdrMaxAge ∷ Monad μ ⇒ CorsResourcePolicy → EitherT String μ HTTP.ResponseHeaders
    hdrMaxAge policy = case corsMaxAge policy of
        Nothing → return []
        Just secs → return [("Access-Control-Max-Age", sshow secs)]

    hdrReqMethod ∷ Monad μ ⇒ CorsResourcePolicy → EitherT String μ HTTP.ResponseHeaders
    hdrReqMethod policy = case lookup "Access-Control-Request-Method" (WAI.requestHeaders r) of
        Nothing → left "Access-Control-Request-Method header is missing in CORS preflight request"
        Just x → if  x `elem` supportedMethods
            then return [("Access-Control-Allow-Methods", hdrL supportedMethods)]
            else left
                $ "Method requested in Access-Control-Request-Method of CORS request is not supported; requested: "
                ⊕ B8.unpack x
                ⊕ "; supported are "
                ⊕ B8.unpack (hdrL supportedMethods)
                ⊕ "."
      where
         supportedMethods = corsMethods policy `union` simpleMethods

    hdrRequestHeader ∷ Monad μ ⇒ CorsResourcePolicy → EitherT String μ HTTP.ResponseHeaders
    hdrRequestHeader policy = case lookup "Access-Control-Request-Headers" (WAI.requestHeaders r) of
        Nothing → return []
        Just hdrsBytes → do
            hdrs ← hoistEither $ AttoParsec.parseOnly httpHeaderNameListParser hdrsBytes
            if hdrs `isSubsetOf` supportedHeaders
                then return [("Access-Control-Allow-Headers", hdrLI supportedHeaders)]
                else left
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
    commonCorsHeaders (Just (o, True)) _  = [("Access-Control-Allow-Origin", o), ("Acdess-Control-Allow-Credentials", "true")]

    -- HTTP response headers that are only used with normal CORS responses
    --
    respCorsHeaders ∷ CorsResourcePolicy → HTTP.ResponseHeaders
    respCorsHeaders policy = catMaybes
        [ fmap (\x → ("Access-Control-Expose-Headers", hdrLI x)) (corsExposedHeaders policy)
        ]

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
httpHeaderNameListParser = P.spaces *> P.sepBy1 (httpHeaderNameParser <* P.spaces) (P.char ',') <* P.spaces

sshow ∷ (IsString α, Show β) ⇒ β → α
sshow = fromString ∘ show

isSubsetOf ∷ Eq α ⇒ [α] → [α] → Bool
isSubsetOf l1 l2 = intersect l1 l2 ≡ l1

-- Add HTTP headers to a WAI response
--
addHeaders ∷ HTTP.ResponseHeaders → WAI.Response → ReqMonad WAI.Response
addHeaders hdrs res = do
#if MIN_VERSION_wai(2,0,0)
    let (st, headers, src) = WAI.responseToSource res
    WAI.responseSource st (headers ⊕ hdrs) <$> src return
#else
    let (st, headers, src) = WAI.responseSource res
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
    ∷ Bool          -- ^ whether to generate a verbose 400 response
    → B8.ByteString -- ^ body
    → WAI.Response
corsFailure True msg = WAI.responseLBS HTTP.status400 [("Content-Type", "text/html; charset-utf-8")] (LB8.fromStrict msg)
corsFailure False _ = WAI.responseLBS HTTP.ok200 [] ""

