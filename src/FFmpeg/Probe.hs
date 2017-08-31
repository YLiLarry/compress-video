{-# LANGUAGE OverloadedStrings #-}

module FFmpeg.Probe where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict  as HM
import           Data.List.Split
import qualified Data.Vector          as V
import           Debug
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           Text.Printf

data Probe = Probe {
    fpath        :: String,
    size         :: Int,
    totalBitRate :: Int,
    codec        :: String,
    bitRate      :: Int,
    frames       :: Integer,
    frameRate    :: Int,
    height       :: Int,
    width        :: Int,
    audioBitRate :: Int,
    duration     :: Float
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
        frames'       <- (fmap read <$> video  .:? "nb_frames")     .!= (-1)
        frameRate'    <- video                  .: "avg_frame_rate"
        height'       <- video                  .: "height"
        width'        <- video                  .: "width"
        duration'     <- read       <$> format  .: "duration"
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
            audioBitRate = audioBitRate' `div` 1000,
            duration     = duration'
        }
        where
            readFrameRate :: String -> Int
            readFrameRate s =
                let [a,b] = splitOn "/" s
                in read a `div` read b

ffprobe :: FilePath -> IO Probe
ffprobe file = do
    ffprobeBin <- getEnv "bin_ffprobe"
    let args = ["-v", "quiet"]
            ++ ["-of", "json"]
            ++ ["-show_format"]
            ++ ["-show_streams"]
            ++ [file]
    errorYellow $ showCommandForUser ffprobeBin args
    (_,Just out,_,h) <- createProcess $ (proc ffprobeBin args) {
        std_out = CreatePipe,
        std_err = Inherit
    }
    b <- B.hGetContents out
    case eitherDecode b of
        Right r -> return r
        Left er -> exitFailure

