module Main where

import Data.SL
import FFmpeg.Config
import FFmpeg.Probe
import FFmpeg.Process
import FFmpeg.Data.H264
import System.Environment
import Control.Monad (void)
import System.FilePath
import Data.Maybe
import System.IO
import System.Power

main :: IO ()
main = do
   suggestPower RequireSystem
   hSetBuffering stdout NoBuffering
   [inPath, outPath, confPath] <- getArgs
   conf <- loadCfg confPath
   info <- ffprobe inPath
   void $ ffmpeg conf info  
   -- setIOFile conf inPath outPath


loadCfg :: FilePath -> IO LoadedCfg
loadCfg path =
   case takeExtension path of
      ".h264" -> LoadedCfg <$> (load path :: IO H264)
      otherwise -> error $ "Undefined extension: " ++ show path
