{-# LANGUAGE ExistentialQuantification #-}

module FFmpeg.Config where
   
import FFmpeg.Probe

class Config a where
   makeArgs :: a -> Probe -> [String]
   defaultCfg :: a

   fullArgs :: a -> Probe -> [String]
   fullArgs a probe =
      ["-n"]
      ++ ["-nostdin"]
      -- ++ ["-no_banner"]
      ++ makeArgs a probe

data LoadedCfg = forall a. (Config a) => LoadedCfg a

instance Config LoadedCfg where
   defaultCfg = undefined
   makeArgs (LoadedCfg a) probe = makeArgs a probe
   
   