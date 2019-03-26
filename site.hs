--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import qualified Data.Text as T (replace, pack, unpack)

import Debug.Trace

--------------------------------------------------------------------------------

pages = [ "about.markdown"
        , "contact.markdown"
        , "misc.markdown"
--        , "research.markdown"
        , "teaching.markdown"
        ]

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList pages) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "cambridge-university-press-numeric.csl" $ compile cslCompiler
    match "Bibliography.bib"    $ compile biblioCompiler

    match "research.markdown" $ do
        route $ setExtension "html"
        compile $
            myPandocBiblioCompiler >>=
            loadAndApplyTemplate "templates/default.html" defaultContext

    match "index.html" $ do
        route idRoute
        compile $ do
            --posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
--                    listField "posts" postCtx (return posts) `mappend`
--                    constField "title" "Home"                `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- TODO: something better than replacing a string in the generated HTML
myPandocBiblioCompiler :: Compiler (Item String)
myPandocBiblioCompiler = do
      csl <- load "cambridge-university-press-numeric.csl"
      bib <- load "Bibliography.bib"
      getResourceBody >>=
          readPandocBiblio defaultHakyllReaderOptions csl bib >>=
          return . fmap replace . writePandoc
  where replace = 
          T.unpack . 
          T.replace "F. Vesely" "<em class=\"author-me\">F. Vesely</em>" . 
          T.pack
