module Main where

import Lib
import FFmpeg
import System.Environment
import Control.Monad (void)
import Cmd

main :: IO ()
main = do
   [input, output] <- getArgs
   void $ ffmpeg defaultCmdArgs (config input output :: H264)

