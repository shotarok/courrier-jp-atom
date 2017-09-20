module CourrierJP where

import Text.HTML.Scalpel hiding (URL)
import Text.Printf
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import qualified Text.XML.Light.Output as XML

------------------------------------------------------------------------------------
-- Scraping Parts
data Article = Article {
  articleId :: String
  , articleLink :: String
  , articlePublished :: String
  , articlePublishedData :: String
  , articleTitle :: String
  , articleCategory ::String
}

instance Show Article where
  show (Article _ link _ published title category) =
    printf "%s <%s> %s %s" published category title link

articleScraper :: Scraper String Article
articleScraper = do
  articleId <- attr "content" $ "meta" @: ["name" @= "cXenseParse:recs:articleid"]
  link      <- attr "content" $ "meta" @: ["property" @= "og:url"]
  published <- attr "content" $ "meta" @: ["name" @= "cXenseParse:recs:publishtime"]
  publishedDate <- text "time"
  title     <- attr "content" $ "meta" @: ["property" @= "og:title"]
  category  <- attr "content" $ "meta" @: ["name" @= "cXenseParse:kod-category"]
  pure $ Article articleId link published publishedDate title category

createArticle :: String ->IO (Maybe Article)
createArticle url = scrapeURL url articleScraper

type ArticleUrl = String

articleUrls :: Scraper String [ArticleUrl]
articleUrls =  chroots ("section" @: [hasClass "column", hasClass "left"] // "li" @: [hasClass "category"])
                       articleUrl

articleUrl :: Scraper String ArticleUrl
articleUrl = do
  link <- attr "href" "a"
  pure link

createArticleList :: String -> IO [Maybe Article]
createArticleList url = do
  res <- scrapeURL url articleUrls
  case res of
    Just urls -> mapM createArticle urls
    Nothing -> return [Nothing]

---------------------------------------------------------------------------
-- Create Atom Feed Parts

createAtom :: [Article] -> String -> String -> String
createAtom articles feedId feedTitle =
  XML.ppElement . Export.xmlFeed $ fd
    { Atom.feedEntries = fmap toEntry articles
    , Atom.feedLinks = [Atom.nullLink courrierJPUrl]
    }
  where
    courrierJPUrl = "https://courrier.jp"
    fd :: Atom.Feed
    fd = Atom.nullFeed feedId           -- Feed ID
           (Atom.TextString feedTitle)  -- Feed Title
           (articlePublished $ head articles) -- Updated

    toEntry :: Article -> Atom.Entry
    toEntry articleScraper =
      (Atom.nullEntry
        (articleId articleScraper)                                      -- Article ID
        (Atom.TextString $ articleTitle articleScraper)                 -- Article Title
        (articlePublished articleScraper))                              -- Article Updated
       { Atom.entryLinks = [Atom.nullLink $ articleLink articleScraper] -- Article Link
       , Atom.entryCategories = [Atom.newCategory $ articleCategory articleScraper] -- Article Category
       }
---------------------------------------------------------------------------
