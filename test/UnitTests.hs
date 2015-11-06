{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: UnitTests
-- Description: Unit Tests for wai-cors
-- Copyright: © 2015 Lars Kuhtz <lakuhtz@gmail.com>.
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
module Main
( main

, post
, patch
, delete
, put
, get
, options
, head
) where

#ifndef MIN_VERSION_wai
#define MIN_VERSION_wai(a,b,c) 1
#endif

import Network.Wai.Middleware.Cors
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import Network.Wai.Test

import Prelude hiding (head)

import Test.Tasty
import Test.Tasty.HUnit

-- -------------------------------------------------------------------------- --
-- Unit tests

main ∷ IO ()
main = defaultMain tests

tests ∷ TestTree
tests = testGroup "unit tests"
    [ testCase "Origins any" test_originsAny
    , testCase "Origins null" test_originsNull
    , testCase "Any Origin" test_anyOrigin
    , testCase "Require Origin" test_requireOrigin
    , testCase "Missing Require Origin" test_missingRequireOrigin
    , testCase "Vary Origin Header" test_varyOriginHeader
    , testCase "Vary Origin No Header" test_varyOriginNoHeader
    ]

test_originsAny ∷ Assertion
test_originsAny = corsSession policy $ do
    resp <- request get
    assertHeader "Access-Control-Allow-Origin" "*" resp
    assertStatus 200 resp
  where
    policy = simpleCorsResourcePolicy
        { corsOrigins = Nothing
        }

test_originsNull ∷ Assertion
test_originsNull = corsSession policy $ do
    resp <- request get
    assertHeader "Access-Control-Allow-Origin" "null" resp
    assertStatus 200 resp
  where
    policy = simpleCorsResourcePolicy
        { corsOrigins = Just (["null"], False)
        }

test_missingRequireOrigin ∷ Assertion
test_missingRequireOrigin = corsSession policy $ do
    resp <- request $ defaultRequest { WAI.requestMethod = "GET" }
    assertStatus 400 resp
  where
    policy = simpleCorsResourcePolicy
        { corsRequireOrigin = True
        }

test_requireOrigin ∷ Assertion
test_requireOrigin = corsSession policy $ do
    resp <- request get
    assertStatus 200 resp
  where
    policy = simpleCorsResourcePolicy
        { corsRequireOrigin = True
        }

test_anyOrigin ∷ Assertion
test_anyOrigin = corsSession policy $ do
    resp <- request get
    assertStatus 200 resp
  where
    policy = simpleCorsResourcePolicy
        { corsOrigins = Nothing
        }

test_varyOriginHeader ∷ Assertion
test_varyOriginHeader = corsSession policy $ do
    resp <- request put
    assertStatus 200 resp
    assertHeader "Vary" "Origin" resp
  where
    policy = simpleCorsResourcePolicy
        { corsOrigins = Just (["null"], False)
        , corsVaryOrigin = True
        }

test_varyOriginNoHeader ∷ Assertion
test_varyOriginNoHeader = corsSession policy $ do
    resp <- request put
    assertStatus 200 resp
    assertNoHeader "Vary" resp
  where
    policy = simpleCorsResourcePolicy
        { corsOrigins = Nothing
        , corsVaryOrigin = True
        }

-- -------------------------------------------------------------------------- --
-- Test Requests

corsSession ∷ CorsResourcePolicy → Session () → Assertion
corsSession policy session = runSession session . cors (const $ Just policy) $ app

corsRequest ∷ WAI.Request
corsRequest = WAI.defaultRequest
    { WAI.pathInfo = ["cors"]
    , WAI.requestHeaders = [("Origin", "null")]
    }

get, post, put, patch, delete, head, options ∷ WAI.Request
get = corsRequest { WAI.requestMethod = "GET" }
post = corsRequest { WAI.requestMethod = "POST" }
put = corsRequest { WAI.requestMethod = "PUT" }
patch = corsRequest { WAI.requestMethod = "PATCH" }
delete = corsRequest { WAI.requestMethod = "DELETE" }
head = corsRequest { WAI.requestMethod = "HEAD" }
options = corsRequest { WAI.requestMethod = "OPTIONS" }

app ∷ WAI.Application
#if MIN_VERSION_wai(2,0,0)
app _ respond = respond $ WAI.responseLBS HTTP.status200 [] "Success"
#else
app _ = WAI.responseLBS HTTP.status200 [] "Success"
#endif

