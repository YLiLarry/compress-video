module Test.FFmpeg.H264 where

import Test.Hspec (hspec, specify, describe, shouldBe)

import FFmpeg.Config as X
import Data.SL
import FFmpeg.Data.H264 as X

test :: IO ()
test = hspec $
   describe "save/load" $ do
      let c = defaultCfg :: H264
      specify "save" $
         save "test/tmp/测试.h264" c
      specify "load" $ do
         obj <- load "test/tmp/测试.h264"
         obj `shouldBe` c

