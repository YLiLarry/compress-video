{-# LANGUAGE ExistentialQuantification #-}

module FFmpeg.Config where

-- import           Control.Monad
-- import           Debug
import           FFmpeg.Probe
import           System.FilePath
import           System.Environment

class Config a where
    makeArgs :: a -> Probe -> [String]
    makeExt :: a -> String
    defaultCfg :: a

    fullArgs :: a -> Probe -> FilePath -> IO [String]
    fullArgs a probe outdir = do
        ffmpegBin <- getEnv "bin_ffmpeg"
        let infile = fpath probe
        return $ [ffmpegBin]
            ++ ["-i", infile]
            ++ ["-n"]
            ++ ["-nostdin"]
            ++ ["-v", "error"]
            ++ ["-progress", fpath probe ++ ".tmp"]
            -- ++ ["-no_banner"]
            ++ makeArgs a probe
            ++ [replaceExtension (replaceDirectory infile outdir) (makeExt a)]

data LoadedCfg = forall a. (Config a) => LoadedCfg a

instance Config LoadedCfg where
    defaultCfg = undefined
    makeArgs (LoadedCfg a) probe = makeArgs a probe
    makeExt (LoadedCfg a) = makeExt a
