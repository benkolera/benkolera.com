--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (pure,(<$>))
import           Control.Monad       ((>=>))
import           Data.Monoid         ((<>))
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match ("favicon.ico" .||. "images/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (baseCtx "about")
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "talks/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/talk.html"    talkCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" talkCtx
            >>= relativizeUrls

    tags <- buildTags allItems $ fromCapture "*.html"

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        route $ customRoute (("tags/" <>) . toFilePath)
        compile $ do
          items <- recentFirst =<< loadAll pattern
          let title = "Posts tagged ‘" ++ tag ++ "’"
          let tagsCtx =
                constField "title" title
                <> listField "items" (baseCtx "tags") (pure items)
                <> baseCtx "tags"
          makeItem ""
            >>= loadAndApplyTemplate "templates/tag-list.html" tagsCtx
            >>= loadAndApplyTemplate "templates/page.html" tagsCtx
            >>= baseTemplate "tags"
    
    create ["tags.html"] $ do
      route idRoute
      compile $ do
        let cloudCtx = constField "title" "Tags of Posts and Talks" <> baseCtx "tags"
        renderTagCloud 100 300 tags
                >>= makeItem
                >>= loadAndApplyTemplate "templates/tag-cloud.html" cloudCtx
                >>= loadAndApplyTemplate "templates/page.html" cloudCtx
                >>= baseTemplate "tags"

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Posts"               <>
                    postCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= baseTemplate "posts"

    create ["drafts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "drafts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Draft Posts"         <>
                    draftCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= baseTemplate "drafs"
    
    create ["talks.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "talks/*"
            let archiveCtx =
                    listField "talks" postCtx (return posts) <>
                    constField "title" "Talks"               <>
                    talkCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/talks.html" archiveCtx
                >>= baseTemplate "talks"

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- take 5 <$> (recentFirst =<< loadAll "posts/*")
            talks <- take 5 <$> (recentFirst =<< loadAll "talks/*")
            tc <- renderTagCloud 75 300 tags
            let indexCtx =
                    listField  "talks" talkCtx (pure talks)  <>
                    listField  "posts" postCtx (pure posts)  <>
                    constField "title" "Home"                <>
                    constField "tag-cloud" tc                <>
                    baseCtx "home"

            getResourceBody >>= applyAsTemplate indexCtx >>= baseTemplate "home"

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedContext = postCtx <> bodyField "description"
        let recentItems = fmap (take 10) . recentFirst
        items <- recentItems =<< loadAllSnapshots allItems "content"
        renderAtom feedConfiguration feedContext items

    match "templates/*" $ compile templateCompiler

    copyBowerComponent "jquery/dist"        "jquery.min.*"         "scripts"
    copyBowerComponent "bootstrap/dist/js"  "bootstrap.min.js"     "scripts"
    copyBowerComponent "bootstrap/dist/css" "bootstrap.css.map"    "css"
    copyBowerComponent "bootstrap/dist/css" "bootstrap.min.css"    "css"
    copyBowerComponent "font-awesome/css"   "font-awesome.min.css" "css"
    copyBowerComponent "font-awesome/css"   "font-awesome.css.map" "css"
    copyBowerComponent "font-awesome/fonts" "*"                    "fonts"

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = baseCtx "posts"

talkCtx :: Context String
talkCtx = baseCtx "talks"

draftCtx :: Context String
draftCtx = baseCtx "drafts"

allItems :: Pattern 
allItems = "posts/*" .||. "talks/*"

baseTemplate :: String -> Item String -> Compiler (Item String)
baseTemplate s = 
  loadAndApplyTemplate "templates/default.html" (baseCtx s)
  >=> relativizeUrls

baseCtx :: String -> Context String
baseCtx s =
  constField ("active-" ++ s) "" 
  <> dateField "date" "%B %e, %Y"
  <> defaultContext

copyBowerComponent :: String -> String -> String -> Rules ()
copyBowerComponent s f t = do
  match (fromGlob ("bower_components/" <> s <> "/" <> f)) $ do
    route $ gsubRoute ("bower_components/" <> s) (const t)
    compile copyFileCompiler

config :: Configuration
config =
  defaultConfiguration {
    deployCommand =
        "rsync --checksum --delete -ave 'ssh' "
        ++ "_site/ ben@benkolera.com:/opt/blog/"
  }
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration {
      feedTitle = "The Functional Web"
    , feedDescription = "The Functional Web -- Tales of a Haskell Web Developer"
    , feedAuthorName = "Ben Kolera"
    , feedAuthorEmail = "ben.kolera@gmail.com"
    , feedRoot = "http://benkolera.com"
  }
