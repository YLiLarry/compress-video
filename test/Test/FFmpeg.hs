module Test.FFmpeg where

import Test.Hspec (hspec, specify, describe)

import FFmpeg

test :: IO ()
test = hspec $
   specify "test/test.in" $ do
      let arg = setIOFile (defaultCfg :: H264) {frames = Just "100"} "test/test.in" "test/test.mp4"
      ffmpeg arg
