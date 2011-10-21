{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty)

import Hakyll

main :: IO ()
main = hakyll $ do
  -- Compress CSS
  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler
  
  -- Copy images
  match "images/**.png" $ do
    route   idRoute
    compile copyFileCompiler

  -- Read templates
  match "templates/*" $ compile templateCompiler

  -- Web pages
  match "**.md" $ do
    route   $ setExtension ".html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/book.html"
      >>> relativizeUrlsCompiler
