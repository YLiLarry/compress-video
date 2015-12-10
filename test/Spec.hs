import Test.FFmpeg as FFmpeg
import Test.FFmpeg.H264 as H264

main :: IO ()
main = do
   H264.test
   FFmpeg.test
