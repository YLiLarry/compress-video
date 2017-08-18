module FFmpeg.Process where

import           Control.Exception
import           Control.Monad.Extra
import           Debug
import           FFmpeg.Config
import           FFmpeg.Probe
import           System.Directory
import           System.Exit
import           System.IO as IO
-- import           System.Power
import           System.Process
import           Text.Printf
import           Text.Regex
import Data.Foldable as F
import Data.Monoid

data FFmpegProcess = FFmpegProcess {
    --  errHandle          :: Handle
     procHandle         :: ProcessHandle
   , cmd                :: String
   , probe              :: Probe
   , progressFileHandle :: Handle
   , progressFilePath   :: FilePath
   , outPath            :: FilePath
   , percentage         :: Float
}

waitForFFmpeg :: ProcessHandle -> IO ExitCode
waitForFFmpeg = waitForProcess


killFFmpeg :: FFmpegProcess -> IO ()
killFFmpeg ffp = do
   let h = procHandle ffp
   interruptProcessGroupOf h
   void $ waitForProcess h
   -- remove file
   let out = outPath ffp
   let tmp = progressFilePath ffp
   whenM (doesFileExist out) (removeFile out)
   whenM (doesFileExist tmp) (removeFile tmp)


ffmpegIsRunning :: FFmpegProcess -> IO Bool
ffmpegIsRunning ffp = do
   maybeExitCode <- getFFmpegExitCode ffp
   return $ case maybeExitCode of
      Just _  -> False
      Nothing -> True


getFFmpegExitCode :: FFmpegProcess -> IO (Maybe ExitCode)
getFFmpegExitCode = getProcessExitCode . procHandle


-- ffmpeg :: Config a => a -> Probe -> IO ()
-- ffmpeg conf probe = do
--    suggestPower RequireSystem
--    pr <- spawnFFmpeg conf probe
--    printFFmpeg pr
--       `catch` onExceptionKill pr
--       `finally` suggestPower Default
--    Just code <- getFFmpegExitCode pr
--    when (code /= ExitSuccess) $ error $ "ffmpeg returns " ++ show code


spawnFFmpeg :: Config a => a -> Probe -> FilePath -> IO FFmpegProcess
spawnFFmpeg config pro outpath = do
    -- make arg
    args <- fullArgs config pro outpath
    exists <- doesFileExist outpath
    when (exists && False) (removeFile outpath)
    when (exists && not True) (errorYellow $ "File " ++ outpath ++ " exists.")

    let p = (shell args) {
          std_out = CreatePipe
        , std_err = CreatePipe
        , std_in  = Inherit
        , create_group = True
    }
    -- print debug info
    errorYellow args
    printPWD
    -- run process
    (_, _, _, pr) <- createProcess p

    -- errp `hSetBinaryMode` True

    let tmpf = fpath pro ++ ".tmp"
    tmphdl <- openFile tmpf ReadWriteMode

    return FFmpegProcess {
        --   errHandle  = errp
          procHandle = pr
        , cmd = args
        , probe = pro
        , progressFileHandle = tmphdl
        , progressFilePath = tmpf
        , outPath = outpath
        , percentage = 0
    }

printPWD :: IO ()
printPWD = do
   pwd <- getCurrentDirectory
   errorYellow $ "pwd: " ++ pwd

printFFmpegProgress :: FFmpegProcess -> IO ()
printFFmpegProgress pc = do
    triple <- getCurrentPercentage pc
    case triple of
        Nothing -> return ()
        Just (total, current, percent) ->
            when (percent > percentage pc)
                (putStrLn $ printf "{\"total\":%.2f, \"current\":%.2f, \"percentage\":%.4f}" total current percent)

getCurrentPercentage :: FFmpegProcess -> IO (Maybe (Float, Float, Float))
getCurrentPercentage pc = do
    ls <- hGetLinesReverse (progressFileHandle pc)
    -- print ls
    return $ do
        current' <- getFirst $ F.foldMap First $ map isDuration ls
        let current = current' / 1000000 :: Float
        let total = duration $ probe pc :: Float
        let percent = (fromInteger $ floor ((current / total) * 10000)) / 10000 :: Float
        return (total, current, percent)

isDuration :: String -> Maybe Float
isDuration str = do
    matched <- matchRegex (mkRegex "out_time_ms=([0-9]+)") str
    Just $ read $ head matched

hGetLinesReverse :: Handle -> IO [String]
hGetLinesReverse = hGetLinesReverse' []
    where
        hGetLinesReverse' sofar hd = do
            eof <- IO.hIsEOF hd
            if eof then return sofar else do
                ready <- IO.hReady hd
                if not ready then return sofar else do
                    l <- hGetLine hd
                    hGetLinesReverse' (l:sofar) hd

onExceptionKill :: FFmpegProcess -> SomeException -> IO ()
onExceptionKill pr e = do
   putStrLn "[Killed on Exception]"
   killFFmpeg pr
   throw e
