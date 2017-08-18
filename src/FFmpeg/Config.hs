{-# LANGUAGE ExistentialQuantification #-}

module FFmpeg.Config where

import           Control.Monad
import           Debug
import           FFmpeg.Probe
import           System.Directory
import           System.Environment

class Config a where
    makeArgs :: a -> Probe -> [String]
    defaultCfg :: a

    fullArgs :: a -> Probe -> FilePath -> IO String
    fullArgs a probe outPath = do
        ffmpegBin <- getEnv "bin_ffmpeg"
        return $ unwords $
               [ffmpegBin]
            ++ ["-i", fpath probe]
            ++ ["-n"]
            ++ ["-nostdin"]
            ++ ["-v", "error"]
            ++ ["-progress", fpath probe ++ ".tmp"]
            -- ++ ["-no_banner"]
            ++ makeArgs a probe
            ++ [outPath]

data LoadedCfg = forall a. (Config a) => LoadedCfg a

instance Config LoadedCfg where
   defaultCfg = undefined
   makeArgs (LoadedCfg a) probe = makeArgs a probe

