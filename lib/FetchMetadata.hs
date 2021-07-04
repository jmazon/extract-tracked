module FetchMetadata
  ( fetchDescription
  , descriptionRule
  , fetchMetadataMain
  )
  where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Development.Shake
import Development.Shake.FilePath
import Network.Google
import Network.Google.YouTube
import System.Environment (getArgs,getProgName,lookupEnv,setEnv)
import System.IO (stderr,hPutStrLn)
import System.Exit

fetchDescription :: Text -> IO (Maybe Text)
fetchDescription vidId = do
  let credsVar = "GOOGLE_APPLICATION_CREDENTIALS"
  lookupEnv credsVar >>= \case
    Just _ -> pure ()
    Nothing -> setEnv credsVar "credentials.json"

  lgr <- newLogger Debug stderr
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ youTubeReadOnlyScope)
  vlr <- runResourceT . runGoogle env $ send (videosList "snippet" & vlId ?~ vidId)
  let title = vlr ^. vlrItems . each . vSnippet . each . vsTitle
      desc = vlr ^. vlrItems . each . vSnippet . each . vsDescription
  pure $ Text.unlines <$> sequence [title,desc]

descriptionRule :: Rules ()
descriptionRule =
  withTargetDocs "Fetch title (first line) and description (remainder) from YT metadata" $
  "*.txt" %> \out -> do
    let videoId = dropExtension out
    liftIO (fetchDescription (Text.pack videoId)) >>= \case
      Nothing -> fail $ "Failed to fetch description for " ++ videoId
      Just description -> liftIO (Text.writeFile out description)

fetchMetadataMain :: IO ()
fetchMetadataMain = getArgs >>= \case
  [videoId] -> do
    fetchDescription (Text.pack videoId) >>= \case
      Just desc -> Text.putStrLn desc
      Nothing   -> do
        hPutStrLn stderr ("No description returned for " ++ videoId)
        exitFailure
  _ -> usage

usage :: IO ()
usage = do
  pgm <- getProgName
  putStrLn $ "Usage: " ++ pgm ++ "<video id>"
  exitWith (ExitFailure 2)
