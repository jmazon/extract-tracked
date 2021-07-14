{-# LANGUAGE NoOverloadedStrings #-}

module Album where

import           Control.DeepSeq
import           Control.Exception (throwIO)
import           Control.Monad
import           Data.Binary
import           Data.Char
import           Data.Foldable
import           Data.Functor
import           Data.Hashable
import           Data.IORef
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Development.Shake
import           Development.Shake.FilePath
import qualified System.Environment as Sys
import           System.IO.Error (isDoesNotExistError)

import           FetchMetadata
import           DescriptionParser

albumMain :: IO ()
albumMain = do
  youtubeDlPath <- fromMaybe "youtube-dl" <$> Sys.lookupEnv "YOUTUBEDL"
  youtubeDlRes <- newResourceIO "youtube-dl" 4
  let youtubeDl = mkYoutubeDl youtubeDlPath youtubeDlRes 3600
  
  rev <- newIORef Map.empty
  shakeArgs shakeOptions $
    descriptionRule <>
    chaptersRule <>
    audioRule youtubeDl <>
    targetPhonies rev <>
    matroskaPhonies rev <>
    matroskaRules "*.webm" rev <>
    matroskaRules "*.mka" rev

mkYoutubeDl :: String -> Resource -> Double
            -> String -> String -> Action ()
mkYoutubeDl path res timeout videoId outFile =
  actionRetry 3 $ withResource res 1 $
      cmd_ (Timeout timeout)
           [ path
           , "-x", "--audio-format", "best"
           , "--exec","mv -- {} " <> outFile
           , "--", videoId
           ]
    `actionCatch` \e -> case isDoesNotExistError e of
                         True -> liftIO $ ioError $ userError "youtube-dl not found.  Set its path in the PATH or its full name in the YOUTUBEDL environment variable."
                         False -> liftIO $ throwIO e

looksLikeVideoId :: String -> Bool
looksLikeVideoId s = map p s == replicate 11 True
  where p c = c >= '0' && c <= '9' ||
              c >= 'A' && c <= 'Z' ||
              c >= 'a' && c <= 'z' ||
              c == '-' || c == '_'

-- | Recognize an 11-character target that lookes like it could be a
-- YT video id to delegate to the appropriate container file.
targetPhonies :: IORef (Map.Map String String) -> Rules ()
targetPhonies rev = withTargetDocs "Perform the whole shebang" $
  phonys $ \videoId -> guard (looksLikeVideoId videoId) $> do
    let metaFile = videoId <.> "txt"
    need [metaFile]
    title <- Text.unpack . head . Text.lines <$> liftIO (Text.readFile metaFile)
    liftIO (modifyIORef rev (Map.insert title videoId))
    let matroskaPhony = matroskaPrefix <> title
    need [matroskaPhony]

matroskaPrefix :: String
matroskaPrefix = "Matroska for "

newtype MatroskaType = MatroskaType String deriving (Eq,Show,Hashable,Binary,NFData)
type instance RuleResult MatroskaType = String

-- | Targets the appropriate container target (webm or generic mka)
-- depending on whether audio contents blend in.  Determines audio
-- type using ffprobe, cached.
matroskaPhonies :: IORef (Map.Map String String) -> Rules ()
matroskaPhonies rev =
  withTargetDocs "Placeholder for Matroska of unknown extension" $ do
    getExt <- addOracleCache $ \(MatroskaType title) -> do
      Just videoId <- liftIO (Map.lookup title <$> readIORef rev)
      let audioFile = videoId <.> "audio"
      need [audioFile]
      Stderr probe <- cmd ["ffprobe","--",audioFile]
      let Just audioType = asum $ map extractAudioType (lines probe)
      pure $ case audioType of
        "opus" -> "webm"
        "aac" -> "mka"
        _ -> error $ "matroskaPhonies: unknown audio type " ++ audioType
    phonys $ \ph -> do
      title <- stripPrefix matroskaPrefix ph
      pure $ do
        ext <- getExt (MatroskaType title)
        need [title <.> ext]

-- This looks like I should have used a regex and duplicated my
-- problem.
extractAudioType :: String -> Maybe String
extractAudioType input = asum $ map f (tails input)
  where f ('A':'u':'d':'i':'o':':':' ':r) = Just (takeWhile isAlpha r)
        f _ = Nothing
    
-- | Generate a Matroska container from audio and chapters.
matroskaRules :: FilePattern -> IORef (Map.Map String String) -> Rules ()
matroskaRules filePat rev = withTargetDocs "Mux to Matroska" $
  filePat %> \outFile -> do
    let title = dropExtension outFile
    Just videoId <- liftIO (Map.lookup title <$> readIORef rev)
    let chaptersFile = videoId <.> "chap"
        audioFile    = videoId <.> "audio"
    need [chaptersFile,audioFile]

    -- mkvmerge doesn't appear to take a "--" separator, but does accept
    -- an input filename that starts with a dash.  Crossing fingers…
    cmd_ ["mkvmerge","--chapters",chaptersFile,"-o",outFile,audioFile]

-- | Generate an audio file by direct YT download.
audioRule :: (String -> String -> Action ()) -> Rules ()
audioRule dl = withTargetDocs "Download audio (best quality) from YT" $
  "*.audio" %> \out -> do
    let videoId = dropExtension out
    dl videoId out
