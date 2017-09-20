import Control.Lens
import Control.Monad
import Data.Text (pack)
import Data.Maybe (catMaybes)
import Network.AWS
import Network.AWS.S3
import System.IO
import CourrierJP


---------------------------------------------------------------------------
-- put S3 object whose read right is granted for all users

putAtomtoS3 :: String -> String -> String -> IO ()
putAtomtoS3 bucketName atomFileName body = do
  e <- newEnv Tokyo $ FromEnv (pack "AWS_ACCESS_KEY_ID") (pack "AWS_SECRET_ACCESS_KEY") (Just $ pack "AWS_SESSION_TOKEN")
  let putObjectReq = putObject (BucketName $ pack bucketName)
                               (ObjectKey $ pack atomFileName)
                               (toBody $ pack body)
                     & poGrantRead .~ Just (pack "uri=http://acs.amazonaws.com/groups/global/AllUsers")
  res <- runResourceT . runAWS e $ send putObjectReq
  putStrLn $ "Status Code:" ++ show (res ^. porsResponseStatus)

-------------------------------------------------------------------------

main :: IO ()
main = do
  let bucketName = "courrier-japon"
  let atomFileName = "atom.xml"
  let feedId = "http://" ++ bucketName ++ ".s3-ap-northeast-1.amazonaws.com/" ++ atomFileName
  let feedTitle = "COURRIER JAPON ATOM FEED"
  let contentPages = ["https://courrier.jp/?page=" ++ show num | num <- [1..5]]
  putStrLn "Creating Feeds..."
  maybeArticleLists <- mapM CourrierJP.createArticleList contentPages -- [[Maybe Article]]
  let maybeArticles = concat maybeArticleLists -- [Maybe Article]
  let articles = catMaybes maybeArticles -- [Article]
  let atomBody = CourrierJP.createAtom articles feedId feedTitle
  putStrLn "Uploading..."
  putAtomtoS3 bucketName atomFileName atomBody
  putStrLn "Done"
