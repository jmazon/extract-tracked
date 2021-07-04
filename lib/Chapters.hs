module Chapters
  ( Chapter(..)
  , serialize
  )
  where

import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import Data.Text.Lazy.Builder
import Text.Printf

data Chapter = Chapter { chapStartMillis :: !Int
                       , chapName :: !Text
                       }
               deriving Show

serialize :: [Chapter] -> LText.Text
serialize chaps
  | length chaps >= 1000 = error "serialize: too many chapters!"
  | otherwise = toLazyText $ mconcat $ zipWith serChap chapNumbers chaps
  where
    serChap n Chapter{..} =
      fromText "CHAPTER" <> n <> "=" <> serTime chapStartMillis <> singleton '\n' <>
      fromText "CHAPTER" <> n <> "NAME=" <> fromText chapName <> singleton '\n'
    chapNumbers | length chaps < 100 = tail [ singleton t <> singleton u
                                            | t <- ['0'..'9'], u <- ['0'..'9'] ]
                | otherwise = tail [ singleton h <> singleton t <> singleton u
                                   | h <- ['0'..'9'], t <- ['0'..'9'], u <- ['0'..'9'] ]
    serTime millis
      | h >= 100 = error "serialize: time too large"
      | otherwise = fromString $ printf "%02d:%02d:%02d.%03d" h m s ms
      where (_s,ms) = millis `divMod` 1000
            (_m,s) = _s `divMod` 60
            (h,m) = _m `divMod` 60
