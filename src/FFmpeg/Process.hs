module FFmpeg.Process where

import           Control.Exception
import           Control.Monad.Extra
import           Control.Monad.Trans.Class as MT
import           Control.Monad.Trans.State as MT
import           Data.Foldable             as F
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Monoid
import           Debug
import           FFmpeg.Config
import           FFmpeg.Probe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO                 as IO
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
   let tmph = progressFileHandle ffp
   when (isJust tmph) (hClose $ fromJust tmph)
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

spawnFFmpeg :: Config a => a -> Probe -> FilePath -> IO FFmpegProcess
spawnFFmpeg config pro outdir = do
    -- make arg
    ffmpegBin <- getEnv "bin_ffmpeg"
    args <- fullArgs config pro outdir
    let strargs = showCommandForUser ffmpegBin args
    let infile = args L.!! 1
    let outfile = last args
    let tmpf = outfile ++ ".tmp"
    when (infile == outfile) $ do
        errorRed "The input file is the same as the output file"
        exitFailure
    oexists <- doesFileExist outfile
    texists <- doesFileExist tmpf
    when texists (removeFile tmpf)
    when (oexists && True) $ do
        errorYellow $ "File " ++ outfile ++ " exists. Will overwrite."
        removeFile outfile
    when (oexists && not True) (errorYellow $ "File " ++ outfile ++ " exists.")

    let p = (proc ffmpegBin args ) {
          std_out = NoStream
        , std_err = Inherit
        , std_in  = CreatePipe
        , create_group = True
    }
    -- print debug info
    errorYellow strargs
    -- run process
    (_, _, _, pr) <- createProcess p
    return FFmpegProcess {
        --   errHandle  = errp
          procHandle = pr
        , cmd = strargs
        , probe = pro
        , progressFileHandle = Nothing
        , progressFilePath = tmpf
        , outPath = outfile
        , percentage = 0
    }

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
    exists <- MT.lift $ doesFileExist fp
    when (isNothing hd && exists) $ do
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
