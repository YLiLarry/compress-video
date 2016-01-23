module Test.FFmpeg where

import Test.Hspec

import System.Process
import FFmpeg
import Data.Maybe
import Control.Exception

test :: IO ()
test = hspec $ do
   describe "Normal test frames = Just 100" $ do
      it "test/test.in" $ do
         let arg = setIOFile (defaultCfg :: H264) {frames = Just "100"} "test/test.in" "test/test.mp4"
         ffmpeg arg
   describe "Orphan test" $ do
      it "Kill immediately" $ do
         let arg = setIOFile (defaultCfg :: H264) {frames = Just "100"} "test/test.in" "test/test.mp4"
         h <- spawnFFmpeg arg
         killFFmpeg h
         code <- getFFmpegExitCode h
         code `shouldSatisfy` isJust 
      it "On Exception Kill" $ do
         let arg = setIOFile (defaultCfg :: H264) {frames = Just "100"} "test/test.in" "test/test.mp4"
         h <- spawnFFmpeg arg
         (`shouldThrow` anyException) $ onExceptionKill h `handle` do
            printFFmpeg h
            throw UserInterrupt
         code <- getFFmpegExitCode h 
         code `shouldSatisfy` isJust         
         
         