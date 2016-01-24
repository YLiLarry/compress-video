module Test.FFmpeg where

import Test.Hspec

import System.Process
import FFmpeg.Config 
import FFmpeg.Probe (ffprobe)
import FFmpeg.Process
import FFmpeg.Data.H264 
import Data.Maybe
import Control.Exception

test :: IO ()
test = hspec $ do
   describe "Normal test/test.in" $ do
      it "frames = Fix 100" $ do
         let arg = (defaultCfg :: H264) {frames = Fix 100} 
         let input = "test/test.in"
         ffmpeg arg =<< (ffprobe input)
      it "frameRate = 10" $ do
         let arg = (defaultCfg :: H264) {frameRate = Max 10} 
         let input = "test/test.in"
         ffmpeg arg =<< (ffprobe input)
      it "bitRate = 10" $ do
         let arg = (defaultCfg :: H264) {frameRate = Max 100, bitRate = Max 1} 
         let input = "test/test.in"
         ffmpeg arg =<< (ffprobe input)
      it "audioBitRate = 10" $ do
         let arg = (defaultCfg :: H264) {frameRate = Max 100, audioBitRate = Max 1} 
         let input = "test/test.in"
         ffmpeg arg =<< (ffprobe input)
      it "size = 10x10" $ do
         let arg = (defaultCfg :: H264) {frameRate = Max 100, height = Max 10, width = Max 10} 
         let input = "test/test.in"
         ffmpeg arg =<< (ffprobe input)
         
   describe "Orphan test" $ do
      it "Kill immediately" $ do
         let arg = (defaultCfg :: H264) {frames = Fix 100} 
         let input = "test/test.in"
         h <- spawnFFmpeg arg =<< (ffprobe input)
         killFFmpeg h
         code <- getFFmpegExitCode h
         code `shouldSatisfy` isJust 
      it "On Exception Kill" $ do
         let arg = (defaultCfg :: H264) {frames = Fix 100} 
         let input = "test/test.in"
         h <- spawnFFmpeg arg =<< (ffprobe input)
         (`shouldThrow` anyException) $ onExceptionKill h `handle` do
            printFFmpeg h
            throw UserInterrupt
         code <- getFFmpegExitCode h 
         code `shouldSatisfy` isJust  
                
   describe "FFProbe" $ do
      it "Read a video" $ do
         probe <- ffprobe "test/test.in"
         print probe
         