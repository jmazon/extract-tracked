module FetchMetadata
  ( fetchDescription
  , descriptionRule
  , fetchMetadataMain
  )
  where

import Control.Exception (handleJust)
import Control.Lens
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Development.Shake
import Development.Shake.FilePath
import Network.Google
import Network.Google.Auth
import Network.Google.YouTube
import System.Environment (getArgs,getProgName,lookupEnv,setEnv)
import System.IO (stderr,hPutStrLn)
import System.Exit

fetchDescription :: Text -> IO (Maybe Text)
fetchDescription vidId = do
  -- That variable is read by gogol, but provide a bit of wrapping to
  -- the possible error messages.
  let credsVar = "GOOGLE_APPLICATION_CREDENTIALS"
  credsFromEnv <- maybe False (const True) <$> lookupEnv credsVar
  unless credsFromEnv $ setEnv credsVar "credentials.json"

  handleJust @ AuthError
    (^? _MissingFileError)
    (\fp -> ioError $ userError $ credsVar <> " envvar or " <> fp <> " file needed")
    $ do
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
