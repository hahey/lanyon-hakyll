--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Control.Monad (liftM)
import qualified Data.Map as M
import           System.FilePath               (takeBaseName)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList $ map fromFilePath pageList) $ do
        route $ setExtension "html"
        compile $ do
            pageName <- takeBaseName . toFilePath <$> getUnderlying
            let pageCtx = metadataField `mappend`
                          constField pageName "" `mappend`
                          constField "page-name" pageName `mappend`
                          baseNodeCtx

            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html"    siteCtx
                >>= loadAndApplyTemplate "templates/default.html" ((sidebarCtx pageCtx) <> siteCtx)
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    constField "archive" "" `mappend`
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    paginate <- buildPaginateWith postsGrouper "posts/*" postsPageId

    paginateRules paginate $ \page pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pattern "content"
            let indexCtx =
                    constField "title" (if page == 1 then "Home"
                                                     else "Blog posts, page " ++ show page) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    constField "home" "" `mappend`
                    paginateContext paginate page `mappend`
                    siteCtx

            makeItem ""
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (evalCtx <>indexCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend`
                    bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderAtom feedConfig feedCtx posts

--------------------------------------------------------------------------------

postsGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
postsGrouper = liftM (paginateEvery 3) . sortRecentFirst

postsPageId :: PageNumber -> Identifier
postsPageId n = fromFilePath $ if (n == 1) then "index.html" else show n ++ "/index.html"

--------------------------------------------------------------------------------

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "lanyon-hakyll: Lanyon Theme on Hakyll"
    , feedDescription = "A Fork of Lanyon based on Poole"
    , feedAuthorName  = "Heuna Kim"
    , feedAuthorEmail = "ai@heuna-kim.net"
    , feedRoot        = "https://github.com/hahey/lanyon-hakyll"
    }

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    bodyField "post-body" `mappend`
    defaultContext

siteCtx :: Context String
siteCtx =
    constField "baseurl" "" `mappend`
    constField "site_description" "Lanyon Theme on Hakyll" `mappend`
    constField "tagline" "A Fork of Lanyon based on Poole" `mappend`
    constField "site-title" "lanyon-hakyll" `mappend`
    constField "copy-year" "2020" `mappend`
    constField "github-repo" "https://github.com/hahey/lanyon-hakyll" `mappend`
    sidebarCtx baseNodeCtx `mappend`
    defaultContext

--------------------------------------------------------------------------------

pageList = [("About.rst" :: String), ("Contact.markdown":: String)]
pair = zip (map fromFilePath pageList) pageList
pages :: [(Identifier, String)] -> [Item String]
pages [] = []
pages ((a,b):xs) = (Item a b):(pages xs)

sidebarCtx :: Context String -> Context String
sidebarCtx nodeCtx =
    evalCtx `mappend`
    listField "list_pages" nodeCtx (return $ pages pair) `mappend`
    defaultContext

baseNodeCtx :: Context String
baseNodeCtx =
    urlField "node-url" `mappend`
    titleField "title" `mappend`
    evalCtx

evalMetadata :: [String] -> Item a -> Compiler String
evalMetadata [key] identifier = getMetadataField' (itemIdentifier identifier) key

evalCtx = functionField "eval" evalMetadata
