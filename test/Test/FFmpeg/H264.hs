module Test.FFmpeg.H264 where

import Test.Hspec (hspec, specify, describe, shouldBe)

import Lib


test :: IO ()
test = hspec $ do
   describe "save/load" $ do
      let c = config "测试/test.in" "test/test.mp4" :: H264
      specify "save" $
         save "test/tmp/测试.h264" c
      specify "load" $ do
         obj <- load "test/tmp/测试.h264"
         obj `shouldBe` c

