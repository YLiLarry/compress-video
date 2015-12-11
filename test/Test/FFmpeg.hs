module Test.FFmpeg where

import Test.Hspec (hspec, specify, describe)

import FFmpeg

test :: IO ()
test = hspec $
   specify "test/test.in" $
      ffmpeg $ setIOFile (defaultCfg :: H264) "test/test.in" "test/test.mp4"

