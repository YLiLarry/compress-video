module FFmpeg.Config where

import Cmd
-- import Data.Maybe

class Config a where
   makeArgs :: a -> [String]
   config :: FilePath -> FilePath -> a

   fullArgs :: CmdArgs -> a -> [String]
   fullArgs _ a =
      ["-y"]
      ++ ["-nostdin"]
      ++ makeArgs a
