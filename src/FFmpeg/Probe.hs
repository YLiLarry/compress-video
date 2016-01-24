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

data Probe = Probe {
   fpath        :: String,
   size         :: Int,
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
      let (Object video)  = V.head streams
      let (Object audio)  = V.last streams
      let (Object format) = o HM.! "format"
      fpath'        <- format .: "filename"  
      size'         <- format .: "size"
      codec'        <- video  .: "codec_name"  
      bitRate'      <- video  .: "bit_rate"
      frames'       <- video  .: "nb_frames"
      frameRate'    <- video  .: "avg_frame_rate"
      height'       <- video  .: "height" 
      width'        <- video  .: "width"
      audioBitRate' <- audio  .: "bit_rate"
      return Probe {
         fpath        = fpath',
         size         = read size',
         codec        = codec',
         bitRate      = read bitRate',
         frames       = read frames',
         frameRate    = readFrameRate frameRate',
         height       = height',
         width        = width',
         audioBitRate = read audioBitRate'
      }      
      where
         readFrameRate :: String -> Int
         readFrameRate s = 
            let [a,b] = splitOn "/" s
            in read a `div` read b
      
ffprobe :: FilePath -> IO Probe
ffprobe file = do 
   (_,Just out,Just err,h) <- createProcess (proc ffprobeBin ["-v", "quiet", "-of", "json", "-show_format", "-show_streams", file]) {
      std_out = CreatePipe,
      std_err = CreatePipe
   }
   waitForProcess h
   s <- hGetContents err
   b <- B.hGetContents out
   let showError e = error $ printf 
         "FFProbe Output:\n%s Error:\n %s\nProbe: cannot read the video info.\nDecode:\n%s\n"
         s (show b) e
   return $ either showError id $ eitherDecode b
            
            
ffprobeBin :: FilePath
ffprobeBin = "ffprobe"

