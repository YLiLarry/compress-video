module FFmpeg.Process where

import FFmpeg.Probe
import FFmpeg.Config
import System.Process
import System.IO
import System.Exit
import System.Directory
import Control.Exception
import Control.Monad
import Text.Regex
import Control.Conditional hiding (when)
import Data.Maybe
import Control.Concurrent
import Data.List.Split
import Text.Printf
import Data.List

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
   proc <- spawnFFmpeg conf probe
   printFFmpeg proc `catch` onExceptionKill proc
   Just code <- getFFmpegExitCode proc
   when (code /= ExitSuccess) $ error $ "ffmpeg returns " ++ show code


spawnFFmpeg :: Config a => a -> Probe -> IO FFmpegProcess
spawnFFmpeg config probe = do
   -- make arg
   let args = fullArgs config probe
   p' <- proc ffmpegBin <$> overwrite False args
   let p = p' {
        std_out = NoStream
      , std_err = CreatePipe
      , std_in  = NoStream
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
   while (\_-> ffmpegIsRunning proc <&&> notM (hIsEOF h)) $ do
      -- threadDelay $ second 0.0001
      currentStr <- matchRegex (mkRegex "time=(.{8})") <$> hGetLine' (errHandle proc)
      when (isJust currentStr) $ do
         let current = parseTime $ head $ fromJust currentStr
         let percent = fromIntegral (100 * current) / fromIntegral total :: Float
         putStrLn $ printf "total=%d current=%d percent=%.2f" total current percent


while :: (() -> IO Bool) -> IO () -> IO ()
while predM doM =
   whenM (predM ()) $ do
      doM
      while predM doM


hGetLine' :: Handle -> IO String
hGetLine' h = do
   c <- hGetChar h
   if c `elem` "\n\r" then return ""
   else (c:) <$> hGetLine' h


parseTime :: String -> Int
parseTime str = read hh * 3600 + read mm * 60 + read ss
   where [hh,mm,ss] = splitOn ":" str


grepDuration :: Handle -> IO String
grepDuration h = fromJust <$> untilM isJust matchDuration Nothing
   where
      matchDuration _ = do
         inStr <- hGetLine h
         let r = flip matchRegex inStr $ mkRegex "Duration: (.{8})"
         return $ head <$> r


untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM pred f acc
   | pred acc = return acc
   | otherwise = do
      x <- f acc
      untilM pred f x


onExceptionKill :: FFmpegProcess -> SomeException -> IO ()
onExceptionKill proc e = do
   putStrLn "[Killed on Exception]"
   killFFmpeg proc
   throw e


second :: Float -> Int
second a = floor $ a * 1000000


overwrite :: Bool -> [String] -> IO [String]
overwrite True args = return args
overwrite False args = do
   bool <- doesFileExist out
   when bool $ error $ printf "File \"%s\" exists." out
   return args
   where out = last args
 