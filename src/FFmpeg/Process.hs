module FFmpeg.Process where

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

data FFmpegProcess = FFmpegProcess {
     errHandle  :: Handle
   , procHandle :: ProcessHandle
}


ffmpegPath :: FilePath
ffmpegPath = "ffmpeg"


waitForFFmpeg :: ProcessHandle -> IO ExitCode
waitForFFmpeg = waitForProcess


killFFmpeg :: FFmpegProcess -> IO ()
killFFmpeg ffp = do 
   let h = procHandle ffp
   interruptProcessGroupOf h
   void $ waitForProcess h


ffmpegIsRunning :: FFmpegProcess -> IO Bool
ffmpegIsRunning ffp = do
   maybeExitCode <- getFFmpegExitCode ffp
   return $ case maybeExitCode of
      Just _  -> False
      Nothing -> True


getFFmpegExitCode :: FFmpegProcess -> IO (Maybe ExitCode)
getFFmpegExitCode = getProcessExitCode . procHandle


ffmpeg :: Config a => a -> IO ()
ffmpeg conf = do
   proc <- spawnFFmpeg conf
   printFFmpeg proc
      `catch` onExceptionKill proc


spawnFFmpeg :: Config a => a -> IO FFmpegProcess
spawnFFmpeg config = do
   -- make arg
   let p = (proc ffmpegPath $ fullArgs config) {
        std_out = NoStream
      , std_err = CreatePipe
      , std_in  = NoStream
   }
   -- print debug info
   printPWD
   printCmd $ cmdspec p
   -- run process
   (_, _, Just errp, proc) <- createProcess p

   return FFmpegProcess {
        errHandle  = errp
      , procHandle = proc
   }


printCmd :: CmdSpec -> IO ()
printCmd (ShellCommand str) = putStrLn $ "cmd: " ++ str
printCmd (RawCommand file args) = putStrLn $ unwords $ ["cmd:", file] ++ args


printPWD :: IO ()
printPWD = do
   pwd <- getCurrentDirectory
   putStrLn $ "pwd: " ++ pwd


printFFmpeg :: FFmpegProcess -> IO ()
printFFmpeg proc = do
   stdout `hSetBuffering` LineBuffering
   let h = errHandle proc
   total <- parseTime <$> grepDuration h
   while (\_-> ffmpegIsRunning proc <&&> notM (hIsEOF h)) $ do
      threadDelay $ second 0.0001
      currentStr <- matchRegex (mkRegex "time=(.{8})") <$> hGetLine' (errHandle proc)
      when (isJust currentStr) $ do
         let current = parseTime $ head $ fromJust currentStr
         let percent = 100 * current `div` total
         putStrLn $ printf "total=%d current=%d percent=%d" total current percent


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
