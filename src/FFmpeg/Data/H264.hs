{-# LANGUAGE DeriveGeneric #-}

module FFmpeg.Data.H264 where

import FFmpeg.Config
import Data.SL
import System.FilePath

data H264 = H264 {
        input     :: FilePath
      , output    :: FilePath
      , frameRate :: String
      , crf       :: String
      , prefix    :: String
      , suffix    :: String
      , frames    :: Maybe String
   } deriving (Generic, Eq, Show)


instance Config H264 where
   defaultCfg = H264 {
           input     = ""
         , output    = ""
         , frameRate = "25"
         , crf    = "18"
         , prefix = "h264_"
         , suffix = ".mp4"
         , frames = Nothing
      }
   makeArgs conf =
      ["-i", input conf, "-codec:v", "libx264", "-crf", crf conf]
      ++ maybe [] (\x -> ["-frames:v", x]) (frames conf)
      ++ [output conf] -- output
   setIOFile a inp _ = a {input = inp, output = outp}
      where outp = takeDirectory inp ++ "/" ++ prefix a ++ takeBaseName inp ++ suffix a

instance SL H264
instance ToJSON H264
instance FromJSON H264

