{-# LANGUAGE ExistentialQuantification #-}

module FFmpeg.Config where

class Config a where
   makeArgs :: a -> [String]
   defaultCfg :: a
   setIOFile :: a -> FilePath -> FilePath -> a

   fullArgs :: a -> [String]
   fullArgs a =
      ["-y"]
      ++ ["-nostdin"]
      ++ makeArgs a


data LoadedCfg = forall a. (Config a) => LoadedCfg a

instance Config LoadedCfg where
   defaultCfg = undefined
   makeArgs (LoadedCfg a) = makeArgs a
   setIOFile (LoadedCfg a) i o = LoadedCfg $ setIOFile a i o