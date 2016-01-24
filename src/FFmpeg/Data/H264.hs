{-# LANGUAGE DeriveGeneric #-}

module FFmpeg.Data.H264 where

import FFmpeg.Config
import qualified FFmpeg.Probe as P
import Data.SL
import System.FilePath
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

unless :: Bool -> [a] -> [a]
unless b a = if b then [] else a

data H264 = H264 {
        frameRate :: Scheme Int
      , crf       :: Int
      , prefix    :: String
      , suffix    :: String
      , frames    :: Scheme Int
      , bitRate   :: Scheme Int
      , audioBitRate :: Scheme Int
      , height    :: Scheme Int
      , width     :: Scheme Int
   } deriving (Generic, Eq, Show)


instance Config H264 where
   defaultCfg = H264 {
           frameRate = Min 25
         , crf    = 18
         , prefix = "h264_"
         , suffix = ".mp4"
         , frames = Keep
         , bitRate      = Keep
         , audioBitRate = Max $ 128 * 1000
         , height = Keep
         , width  = Keep
      }
   makeArgs conf probe = []
      ++ unless (keep $ frameRate conf) 
            ["-r:v", show $ get (frameRate conf) (P.frameRate probe)]
      ++ ["-i", input]
      ++ ["-codec:v", "libx264"]
      ++ ["-strict", "-2", "-codec:a", "aac"]
      ++ ["-crf", show $ crf conf]
      ++ unless (keep $ bitRate conf) 
            ["-b:v", show $ get (bitRate conf) (P.bitRate probe)]
      ++ unless (keep $ audioBitRate conf) 
            ["-b:a", show $ get (audioBitRate conf) (P.audioBitRate probe)]
      ++ unless (keep $ frames conf) 
            ["-frames:v", show $ get (frames conf) (P.frames probe)]
      ++ unless (keep (width conf) && keep (height conf)) 
            ["-s:v", printf "%dx%d" 
                        (get (width conf) (P.width probe)) 
                        (get (height conf) (P.height probe))]
      ++ [output]
      where
         input = P.fpath probe
         output = takeDirectory input ++ "/" ++ prefix conf ++ takeBaseName input ++ suffix conf

instance SL H264
instance ToJSON H264
instance FromJSON H264

