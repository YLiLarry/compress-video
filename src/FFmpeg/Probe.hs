{-# LANGUAGE OverloadedStrings #-}

module FFmpeg.Probe where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import Data.Either
import System.Process
import Text.Printf
import System.IO
import Data.List.Split
import Data.Maybe

data Probe = Probe {
   fpath        :: String,
   size         :: Int,
   totalBitRate :: Int,
   codec        :: String,
   bitRate      :: Int,
   frames       :: Int,
   frameRate    :: Int,
   height       :: Int,
   width        :: Int,
   audioBitRate :: Int
} deriving (Show)

instance FromJSON Probe where
   parseJSON (Object o) = do
      let (Array streams) = o HM.! "streams"
      (Object video) <- maybe (fail "Cannot find a video stream.") return $ 
         V.find (\(Object video) -> "video" == video HM.! "codec_type") streams
      (Object audio) <- maybe (fail "Cannot find an audio stream.") return $ 
         V.find (\(Object audio) -> "audio" == audio HM.! "codec_type") streams
      let (Object format) = o HM.! "format"
      fpath'        <- format                 .: "filename"  
      size'         <- read       <$> format  .: "size"
      totalBitRate' <- read       <$> format  .: "bit_rate"
      audioBitRate' <- read       <$> audio   .: "bit_rate"
      bitRate'      <- (fmap read <$> video  .:? "bit_rate")      .!= (totalBitRate' - audioBitRate') 
      codec'        <- video                  .: "codec_name"  
      frames'       <- read       <$> video  .:? "nb_frames"      .!= "0"
      frameRate'    <- video                  .: "avg_frame_rate"
      height'       <- video                  .: "height" 
      width'        <- video                  .: "width"
      return Probe {
         fpath        = fpath',
         size         = size',
         codec        = codec',
         totalBitRate = totalBitRate' `div` 1000,
         bitRate      = bitRate' `div` 1000,
         frames       = frames',
         frameRate    = readFrameRate frameRate',
         height       = height',
         width        = width',
         audioBitRate = audioBitRate' `div` 1000
      }      
      where
         readFrameRate :: String -> Int
         readFrameRate s = 
            let [a,b] = splitOn "/" s
            in read a `div` read b
      
ffprobe :: FilePath -> IO Probe
ffprobe file = do 
   let args = ([]
         ++ ["-v", "quiet"]
         ++ ["-of", "json"]
         ++ ["-show_entries", "format=filename,size,bit_rate:stream=codec_name,codec_type,bit_rate,nb_frames,avg_frame_rate,height,width"]
         ++ [file])
   (_,Just out,Just err,h) <- createProcess (proc ffprobeBin args) {
         std_out = CreatePipe,
         std_err = CreatePipe
      }
   waitForProcess h
   s <- hGetContents err
   b <- B.hGetContents out
   let showError e = error $ printf 
         "FFProbe Output:\n%s Error: ffprobe %s\n %s\nProbe: Cannot read the video info.\nDecode: %s\n"
         s (unwords args) (show b) e
   return $ either showError id $ eitherDecode b
            
            
ffprobeBin :: FilePath
ffprobeBin = "ffprobe"

