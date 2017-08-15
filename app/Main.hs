module Main where

import           Control.Monad      (void)
import           Data.Maybe
import           Data.SL
import           FFmpeg.Config
import           FFmpeg.Data.H264
import           FFmpeg.Probe
import           FFmpeg.Process
import           System.Environment
import           System.FilePath
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        [inPath, outPath, confPath] -> do
            conf <- loadCfg confPath
            info <- ffprobe inPath
            void $ ffmpeg conf info
        _ -> putStrLn "[input path] [output path] [config path]"
    -- setIOFile conf inPath outPath


loadCfg :: FilePath -> IO LoadedCfg
loadCfg path =
    case takeExtension path of
        ".h264" -> LoadedCfg <$> (load path :: IO H264)
        _       -> error $ "Undefined extension: " ++ show path
