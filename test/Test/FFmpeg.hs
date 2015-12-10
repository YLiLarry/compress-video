module Test.FFmpeg where

import Test.Hspec (hspec, specify, describe)

import FFmpeg
import Cmd

test :: IO ()
test = hspec $ do
   specify "test/test.in" $ do
      ffmpeg defaultCmdArgs (config "test/test.in" "test/test.mp4" :: H264) >>= print

