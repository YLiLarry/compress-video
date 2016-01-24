{-# LANGUAGE DeriveGeneric #-}

module FFmpeg.Data.H264 where

import FFmpeg.Config
import qualified FFmpeg.Probe as P
import Data.SL
import System.FilePath

data Scheme a = Fix a
              | Min a
              | Max a
              | Keep
              deriving (Generic, Eq, Show)
instance (ToJSON a) => ToJSON (Scheme a)
instance (FromJSON a) => FromJSON (Scheme a)

arg :: (Ord a, Show a) => String -> Scheme a -> a -> [String]
arg str (Fix a) _ = [str, show a]
arg str (Min a) b = [str, show $ max a b]
arg str (Max a) b = [str, show $ min a b]
arg _ Keep _ = []

data H264 = H264 {
        frameRate :: Scheme Int
      , crf       :: Int
      , prefix    :: String
      , suffix    :: String
      , frames    :: Scheme Int
   } deriving (Generic, Eq, Show)


instance Config H264 where
   defaultCfg = H264 {
           frameRate = Min 25
         , crf    = 18
         , prefix = "h264_"
         , suffix = ".mp4"
         , frames = Keep
      }
   makeArgs conf probe = []
      ++ arg "-r:v" (frameRate conf) (P.frameRate probe)
      ++ ["-i", input]
      ++ ["-codec:v", "libx264"]
      ++ ["-crf", show $ crf conf]
      ++ arg "-frames:v" (frames conf) (P.frames probe)
      ++ [output]
      where
         input = P.fpath probe
         output = takeDirectory input ++ "/" ++ prefix conf ++ takeBaseName input ++ suffix conf

instance SL H264
instance ToJSON H264
instance FromJSON H264

