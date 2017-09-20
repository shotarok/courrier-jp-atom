
import Data.Char (isSpace)
import Test.HUnit
import CourrierJP

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

trim_lines :: String -> String
trim_lines = \xml -> concat $ map (\line -> trim line) (lines xml)

createAtomTest :: Test
createAtomTest = "test1" ~: expected ~=? got
  where
    article :: CourrierJP.Article
    article = Article "id" "link" "published" "published_date" "title" "category"

    got_xml = CourrierJP.createAtom [article] "feed_id" "feed_title"
    got = trim_lines got_xml

    expected_xml = "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n\
    \  <title type=\"text\">feed_title</title>\n\
    \  <id>feed_id</id>\n\
    \  <updated>published</updated>\n\
    \  <link href=\"https://courrier.jp\" />\n\
    \  <entry>\n\
    \    <id>id</id>\n\
    \    <title type=\"text\">title</title>\n\
    \    <updated>published</updated>\n\
    \    <category term=\"category\" label=\"category\" />\n\
    \    <link href=\"link\" />\n\
    \  </entry>\n\
    \</feed>"
    expected = trim_lines expected_xml

main :: IO ()
main = do
  runTestTT $ TestList [createAtomTest]
  return ()
