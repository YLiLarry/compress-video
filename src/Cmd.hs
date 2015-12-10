module Cmd where

data CmdArgs = CmdArgs {
   progressFile :: Maybe FilePath
}

defaultCmdArgs :: CmdArgs
defaultCmdArgs = CmdArgs {
   progressFile = Just "./tmp/progress"
}
