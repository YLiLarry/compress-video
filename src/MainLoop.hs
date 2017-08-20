module MainLoop where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class  as MT
import           Control.Monad.Trans.State as MT
import           Data.SL
import           Debug
import           FFmpeg.Config
import           FFmpeg.Data.H264
import           FFmpeg.Probe
import           FFmpeg.Process
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
-- import Control.Exception

mainLoop :: IO ()
mainLoop = handle onAnyException $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    args <- getArgs
    case args of
        [inPath, opath, confPath] -> do
            if (inPath == opath) then do
                errorYellow "The input path must be different from the output path."
                errorYellow "[input path] [output path] [config path]"
                exitFailure
            else do
                conf <- loadCfg confPath
                info <- ffprobe inPath
                hl <- spawnFFmpeg conf info opath
                evalStateT loop hl
        _ -> do
            errorYellow "[input path] [output path] [config path]"
            exitFailure

    -- setIOFile conf inPath outPath

onAnyException :: SomeException -> IO ()
onAnyException e = do
    threadDelay 1000000
    throw e

loadCfg :: FilePath -> IO LoadedCfg
loadCfg path =
    case takeExtension path of
        ".h264" -> LoadedCfg <$> (load path :: IO H264)
        _       -> error $ "Undefined extension: " ++ show path


loop :: StateT FFmpegProcess IO ()
loop = forever $ do
    checkCommand
    checkProgress
    MT.lift $ threadDelay (1000000 `div` 10)

checkCommand :: StateT FFmpegProcess IO ()
checkCommand = do
    ready <- MT.lift $ hReady stdin
    when ready $ do
        str <- MT.lift getLine
        case str of
            "quit" -> shutdown
            _ -> return ()

checkProgress :: StateT FFmpegProcess IO ()
checkProgress = do
    rd <- MT.get
    code <- MT.lift $ getProcessExitCode (procHandle rd)
    case code of
        Nothing -> printFFmpegProgress
        Just k -> MT.lift $ exitWith k

shutdown :: StateT FFmpegProcess IO ()
shutdown = do
    rd <- MT.get
    MT.lift $ do
        errorYellow "Shutting down ffmpeg..."
        killFFmpeg rd
        exitFailure
