{-# LANGUAGE DeriveGeneric #-}

module FFmpeg.Data.H264 where

import FFmpeg.Config
import qualified FFmpeg.Probe as P
import Data.SL
import Text.Printf

data Scheme a = Fix a
              | Min a
              | Max a
              | Keep
              deriving (Generic, Eq, Show)
instance (ToJSON a) => ToJSON (Scheme a)
instance (FromJSON a) => FromJSON (Scheme a)

get :: (Ord a, Show a) => Scheme a -> a -> a
get (Fix a) _ = a
get (Min a) b = max a b
get (Max a) b = min a b


keep :: (Eq a) => Scheme a -> Bool
keep = (Keep ==)

unlessL :: Bool -> [a] -> [a]
unlessL b a = if b then [] else a

whenL :: Bool -> [a] -> [a]
whenL b a = if b then a else []

data H264 = H264 {
        frameRate :: Scheme Int
      , crf       :: Int
      , prefix    :: String
      , suffix    :: String
      , frames    :: Scheme Integer
      , bitRate   :: Scheme Int
      , audioBitRate :: Scheme Int
      , height    :: Scheme Int
      , width     :: Scheme Int
   } deriving (Generic, Eq, Show)


instance Config H264 where
   defaultCfg = H264 {
           frameRate = Max 25
         , crf    = 18
         , prefix = "h264_"
         , suffix = ".mp4"
         , frames = Keep
         , bitRate      = Keep
         , audioBitRate = Max 128
         , height = Keep
         , width  = Keep
      }
   makeArgs conf probe = []
      -- output
      ++ ["-codec:v", "libx264"]
      ++ ["-strict", "-2", "-codec:a", "aac", "-async", "1000"]
      ++ whenL (keep $ bitRate conf) ["-crf", show $ crf conf]
      ++ unlessL (keep $ bitRate conf)
            ["-b:v", printf "%dk" $ get (bitRate conf) (P.bitRate probe)]
      ++ unlessL (keep $ audioBitRate conf)
            ["-b:a", printf "%dk" $ get (audioBitRate conf) (P.audioBitRate probe)]
      ++ unlessL (keep $ frames conf)
            ["-frames:v", show $ get (frames conf) (P.frames probe)]
      ++ unlessL (keep (width conf) && keep (height conf))
            ["-s:v", printf "%dx%d"
                        (get (width conf) (P.width probe))
                        (get (height conf) (P.height probe))]
      ++ unlessL (keep $ frameRate conf)
            ["-r:v", show $ get (frameRate conf) (P.frameRate probe)]

instance SL H264
instance ToJSON H264
instance FromJSON H264

