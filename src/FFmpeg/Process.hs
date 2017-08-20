module FFmpeg.Process where

import           Control.Exception
import           Control.Monad.Extra
import           Debug
import           FFmpeg.Config
import           FFmpeg.Probe
import           System.Directory
import           System.Exit
import           System.IO                 as IO
-- import           System.Power
import           Control.Monad.Trans.Class as MT
import           Control.Monad.Trans.State as MT
import           Data.Foldable             as F
import           Data.Maybe
import           Data.Monoid
import           System.Process
import           Text.Printf
import           Text.Regex

data FFmpegProcess = FFmpegProcess {
    --  errHandle          :: Handle
     procHandle         :: ProcessHandle
   , cmd                :: String
   , probe              :: Probe
   , progressFileHandle :: Maybe Handle
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
    let tmpf = fpath pro ++ ".tmp"
    -- make arg
    args <- fullArgs config pro outpath
    oexists <- doesFileExist outpath
    texists <- doesFileExist tmpf
    when texists (removeFile tmpf)
    when (oexists && True) (removeFile outpath)
    when (oexists && not True) (errorYellow $ "File " ++ outpath ++ " exists.")

    let p = (shell args) {
          std_out = NoStream
        , std_err = Inherit
        , std_in  = CreatePipe
    }
    -- print debug info
    errorYellow args
    printPWD
    -- run process
    (_, _, _, pr) <- createProcess p


    return FFmpegProcess {
        --   errHandle  = errp
          procHandle = pr
        , cmd = args
        , probe = pro
        , progressFileHandle = Nothing
        , progressFilePath = tmpf
        , outPath = outpath
        , percentage = 0
    }

printPWD :: IO ()
printPWD = do
   pwd <- getCurrentDirectory
   errorYellow $ "pwd: " ++ pwd

printFFmpegProgress :: StateT FFmpegProcess IO ()
printFFmpegProgress = do
    pc <- MT.get
    let fp = progressFilePath pc
    exists <- MT.lift $ doesFileExist fp
    when exists $ do
        triple <- getCurrentPercentage
        case triple of
            Nothing -> return ()
            Just (total, current, percent) ->
                when (percent > percentage pc)
                    (MT.lift $ putStrLn $ printf "{\"total\":%.2f, \"current\":%.2f, \"percentage\":%.4f}" total current percent)

getCurrentPercentage :: StateT FFmpegProcess IO (Maybe (Float, Float, Float))
getCurrentPercentage = do
    oldpc <- MT.get
    let fp = progressFilePath oldpc
    let hd = progressFileHandle oldpc
    when (isNothing hd) $ do
        hd' <- MT.lift $ openFile fp ReadMode
        MT.put oldpc {
            progressFileHandle = Just hd'
        }
    newpc <- MT.get
    ls <- MT.lift $ hGetLinesReverse (fromJust $ progressFileHandle newpc)
    return $ do
    -- print ls
        current' <- getFirst $ F.foldMap First $ map isDuration ls
        let current = current' / 1000000 :: Float
        let total = duration $ probe newpc :: Float
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
