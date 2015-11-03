{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Main
-- Description: Test suite that uses PhantomJS to simulate a browser
-- Copyright: © 2015 Lars Kuhtz <lakuhtz@gmail.com>
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
module Main
( main
) where

import Control.Concurrent

import Data.Monoid.Unicode
import Data.String

import System.Directory
import System.FilePath
import System.Exit
import System.IO
import System.Process

-- internal modules

import qualified Server as Server

phantomJsBinaryPath ∷ FilePath
phantomJsBinaryPath = "phantomjs"

phantomJsArgs ∷ IsString a ⇒ [a]
phantomJsArgs = ["--ignore-ssl-errors=true"]
-- phantomJsArgs = ["--ignore-ssl-errors=true", "--debug=true"]

phantomJsScriptPath ∷ FilePath
phantomJsScriptPath = "test/phantomjs.js"

indexFilePath ∷ FilePath
indexFilePath = "test/index.html"

runPhantomJs ∷ IO ()
runPhantomJs = do

    -- check that phantomJS binary is available
    -- FIXME check the version
    findExecutable phantomJsBinaryPath >>= \case
        Nothing → do
            hPutStrLn stderr $ "Missing PhantomJS exectuable: in order to run this test-suite PhantomJS must be availabe on the system"
            exitFailure
        Just _ → return ()

    pwd ← getCurrentDirectory
    -- FIXME I consider it an API bug of the directory package that in order
    -- to get the exit code we have also capture stdout and stderr.
    (code, out, err) ← readProcessWithExitCode phantomJsBinaryPath (args pwd) ""
    hPutStrLn stdout out
    hPutStrLn stderr err
    exitWith code
  where
    args pwd = phantomJsArgs ⊕ [phantomJsScriptPath] ⊕ [pwd </> indexFilePath]

main ∷ IO ()
main = do
    _ ← forkIO $ Server.main
    runPhantomJs
