module Test.FFmpeg where

import Test.Hspec

import System.Process
import FFmpeg
import Data.Maybe

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
         code <- getProcessExitCode (procHandle h) 
         code `shouldSatisfy` isJust 
         