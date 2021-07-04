module DescriptionParser (parseDescription,chaptersRule) where

import           Control.Applicative ((<|>))
import           Control.Monad       (guard,(<=<))
import           Data.Char           (isDigit)
import           Data.Foldable       (foldl',toList,asum)
import           Data.Function       (on)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict    as Map
import           Data.Maybe          (fromMaybe,mapMaybe)
import           Data.Semigroup      (Arg(Arg),Max(Max))
import           Data.Text           (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO       as Text
import qualified Data.Text.Lazy.IO  as LText
import qualified Data.Text.Read     as Text
import           Development.Shake
import           Development.Shake.FilePath
import           Text.Regex.PCRE     (Regex,makeRegex,matchM,getAllTextSubmatches)

import Chapters

parseDescription :: Text -> [Chapter]
parseDescription desc = mapMaybe m2c matches
  where
    re = makeRegex ("(" <> tsRe <> ")(.*)") :: Regex
    tsRe = "(?:[0-9]{1,2}:){1,2}[0-9]{1,2}(?:\\.[0-9]+)?" :: String
    matches =
      mapMaybe (mapM (exceptToMaybe . Text.decodeUtf8') . tail . getAllTextSubmatches
                <=< matchM re . Text.encodeUtf8)
        (Text.lines desc)
    Just longestCommonPrefix = longestPrefix ((!! 1) <$> matches)

    m2c [ts,title] =
      Chapter (parseTs ts) <$> Text.stripPrefix longestCommonPrefix title
    m2c _ = error "parseDescription/m2c internal error: too many submatches"

chaptersRule :: Rules ()
chaptersRule = withTargetDocs "Generate OGM chapters file from metadata" $
  "*.chap" %> \out -> do
    let descFile = out -<.> "txt"
    need [descFile]
    desc <- liftIO (Text.readFile descFile)
    let chaps = parseDescription desc
    liftIO $ guard (not (null chaps))
    liftIO (LText.writeFile out (serialize chaps))

parseTs :: Text -> Int
parseTs = go 0 . Text.groupBy ((==) `on` isDigit) where
  go acc (n : ":" : r) = let Right (n',_) = Text.decimal n in go (60 * (n' + acc)) r
  go acc [n,".",m] =
    let Right (n',_) = Text.decimal n
        Right (m',_) = Text.decimal m
    in 1000 * (acc + n') + m'
  go acc [n] = let Right (n',_) = Text.decimal n in 1000 * (acc + n')
  go _ _ = error "parseTs internal error: bad input"

exceptToMaybe :: Either a b -> Maybe b
exceptToMaybe = either (const Nothing) Just

-- The longest prefix is detected as the longest one among those who
-- have the most occurrences.  We detect that by annotating a prefix
-- tree with the number of paths that traverse it, finding the number
-- that occurs most, then the longest occurence along that number.

longestPrefix :: Foldable f => f Text -> Maybe Text
longestPrefix ts = Text.pack <$> foldTrie findFreq trie
  where
    trie :: Trie Int
    trie = foldl' (flip insert) trieEmpty ts

    -- freq -> # of occurrences
    freqMap :: IntMap.IntMap Int
    freqMap = IntMap.fromListWith (+) (map (,1) (toList trie))

    Just (Max (Arg mostCommonFreq _freqCount)) =
      foldMap (Just . Max . uncurry (Arg)) (IntMap.assocs freqMap)

    findFreq :: Int -> [(Char,Maybe String)] -> Maybe String
    findFreq freq hps =
      asum (map (uncurry (fmap . (:))) hps)
      <|> ([] <$ guard (freq == mostCommonFreq))

data Trie a = Trie !a !(Map.Map Char (Trie a)) deriving (Show,Foldable)

trieEmpty :: Trie Int
trieEmpty = Trie 0 Map.empty

insert :: Text -> Trie Int -> Trie Int
insert s (Trie c m) = Trie (c+1) $ case Text.uncons s of
                 Nothing -> m
                 Just (h,t) -> Map.alter (Just . insert t . fromMaybe trieEmpty) h m

foldTrie :: (a -> [(Char,b)] -> b) -> Trie a -> b
foldTrie f = go where
  go (Trie c m) = f c (Map.assocs (go <$> m))
