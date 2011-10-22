{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty)
import Text.Pandoc (WriterOptions(..), defaultParserState, defaultWriterOptions)
import Text.Pandoc.Shared (HTMLMathMethod(..))

import Hakyll

main :: IO ()
main = hakyll $ do
  -- Compress CSS
  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler
  
  -- Copy images
  match images $ do
    route   idRoute
    compile copyFileCompiler

  -- Read templates
  match "templates/*" $ compile templateCompiler

  -- General pages
  match (list ["index.md"]) $ do
    route   $ setExtension ".html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/book.html"
      >>> relativizeUrlsCompiler

  -- Chapters
  match (list chapters) $ do
    route   $ setExtension ".html"
    compile $ pageCompilerWith defaultParserState chapterOptions
      >>> applyTemplateCompiler "templates/book.html"
      >>> relativizeUrlsCompiler

chapterOptions = defaultWriterOptions {
                   writerHTMLMathMethod  = MathML Nothing,
                   writerNumberSections  = True,
                   writerSectionDivs     = True,
                   writerStandalone      = True,
                   writerTableOfContents = True,
                   writerTemplate        = "<b>Table of contents</b>\n$toc$\n$body$"
                 }

chapters = ["Intro/index.md", "Words/index.md", "NGrams/index.md"]
images = regex "(\\.svg$)|(\\.png$)"
