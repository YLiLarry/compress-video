module FFmpeg.Process where

import FFmpeg.Probe
import FFmpeg.Config
import System.Process
import System.IO
import System.Exit
import System.Directory
import Control.Exception
import Control.Monad.Extra
import Text.Regex
import Data.Maybe
import Control.Concurrent
import Data.List.Split
import Text.Printf
import Data.List
import System.Power

data FFmpegProcess = FFmpegProcess {
     errHandle  :: Handle
   , procHandle :: ProcessHandle
   , cmd        :: [String]
}


ffmpegBin :: FilePath
ffmpegBin = "ffmpeg"


waitForFFmpeg :: ProcessHandle -> IO ExitCode
waitForFFmpeg = waitForProcess


killFFmpeg :: FFmpegProcess -> IO ()
killFFmpeg ffp = do 
   let h = procHandle ffp
   interruptProcessGroupOf h
   void $ waitForProcess h
   -- remove file
   let out = last $ cmd ffp
   whenM (doesFileExist out) (removeFile out)


ffmpegIsRunning :: FFmpegProcess -> IO Bool
ffmpegIsRunning ffp = do
   maybeExitCode <- getFFmpegExitCode ffp
   return $ case maybeExitCode of
      Just _  -> False
      Nothing -> True


getFFmpegExitCode :: FFmpegProcess -> IO (Maybe ExitCode)
getFFmpegExitCode = getProcessExitCode . procHandle


ffmpeg :: Config a => a -> Probe -> IO ()
ffmpeg conf probe = do
   suggestPower RequireSystem
   proc <- spawnFFmpeg conf probe
   printFFmpeg proc 
      `catch` onExceptionKill proc 
      `finally` suggestPower Default
   Just code <- getFFmpegExitCode proc
   when (code /= ExitSuccess) $ error $ "ffmpeg returns " ++ show code


spawnFFmpeg :: Config a => a -> Probe -> IO FFmpegProcess
spawnFFmpeg config probe = do
   -- make arg
   let args = fullArgs config probe
   p' <- proc ffmpegBin <$> overwrite False args
   let p = p' {
        std_out = Inherit
      , std_err = CreatePipe
      , std_in  = Inherit
      , create_group = True
   }
   -- print debug info
   printCmd $ cmdspec p
   printPWD
   -- run process
   (_, _, Just errp, proc) <- createProcess p
   
   errp `hSetBinaryMode` True
   
   return FFmpegProcess {
        errHandle  = errp
      , procHandle = proc
      , cmd = ffmpegBin : args
   }


printCmd :: CmdSpec -> IO ()
printCmd (ShellCommand str) = putStrLn $ "cmd: " ++ str
printCmd (RawCommand file args) = 
   putStrLn $ unwords $ ["cmd:", file] ++ args

printPWD :: IO ()
printPWD = do
   pwd <- getCurrentDirectory
   putStrLn $ "pwd: " ++ pwd


printFFmpeg :: FFmpegProcess -> IO ()
printFFmpeg proc = do
   let h = errHandle proc
   total <- parseTime <$> grepDuration h
   let printProgress = do
         currentStr <- matchRegex regex <$> hGetLine' (errHandle proc)
         when (isJust currentStr) $ do
            let current = parseTime $ head $ fromJust currentStr
            let percent = fromIntegral (100 * current) / fromIntegral total :: Float
            putStrLn $ printf "total=%d current=%d percent=%.2f" total current percent
         whenM (ffmpegIsRunning proc &&^ notM (hIsEOF h)) printProgress
   printProgress
   where regex = mkRegex "time=(.{8})"

hGetLine' :: Handle -> IO String
hGetLine' h = do
   c <- hGetChar h
   if c `elem` "\n\r" then return ""
   else (c:) <$> hGetLine' h


parseTime :: String -> Int
parseTime str = read hh * 3600 + read mm * 60 + read ss
   where [hh,mm,ss] = splitOn ":" str


grepDuration :: Handle -> IO String
grepDuration h = do
   inStr <- hGetLine h
   let r = matchRegex regex inStr
   if isJust r then return $ head $ fromJust r else grepDuration h
   where regex = mkRegex "Duration: (.{8})"


onExceptionKill :: FFmpegProcess -> SomeException -> IO ()
onExceptionKill proc e = do
   putStrLn "[Killed on Exception]"
   killFFmpeg proc
   throw e

overwrite :: Bool -> [String] -> IO [String]
overwrite True args = return args
overwrite False args = do
   bool <- doesFileExist out
   when bool $ error $ printf "File \"%s\" exists." out
   return args
   where out = last args
 