{-# LANGUAGE ExistentialQuantification #-}

module FFmpeg.Config where

-- import           Control.Monad
-- import           Debug
import           FFmpeg.Probe
import           System.Environment
import           System.FilePath

class Config a where
    makeArgs :: a -> Probe -> [String]
    makeExt :: a -> String
    defaultCfg :: a

    fullArgs :: a -> Probe -> FilePath -> IO [String]
    fullArgs a probe outdir = do
        let infile = fpath probe
        let outfile = replaceExtension (replaceDirectory infile outdir) (makeExt a)
        return $ ["-i", infile]
              ++ ["-n"]
              ++ ["-nostdin"]
              ++ ["-v", "error"]
              ++ ["-progress", outfile ++ ".tmp"]
              ++ makeArgs a probe
              ++ [outfile]

data LoadedCfg = forall a. (Config a) => LoadedCfg a

instance Config LoadedCfg where
    defaultCfg = undefined
    makeArgs (LoadedCfg a) probe = makeArgs a probe
    makeExt (LoadedCfg a) = makeExt a
