module Main where

import Lib
import FFmpeg
import System.Environment
import Control.Monad (void)
import System.FilePath
import Data.Maybe

main :: IO ()
main = do
   [input, output, conf] <- getArgs
   file <- loadCfg conf
   void $ ffmpeg $ setIOFile file input output


loadCfg :: FilePath -> IO LoadedCfg
loadCfg path =
   case takeExtension path of
      ".h264" -> LoadedCfg <$> (load path :: IO H264)
      otherwise -> error $ "Undefined extension: " ++ show path
