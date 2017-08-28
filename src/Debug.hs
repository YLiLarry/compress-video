module Debug where

import           System.Console.ANSI
import           System.IO

errorYellow :: String -> IO ()
errorYellow str = do
    hSetSGR stderr [SetColor Foreground Vivid Yellow]
    hPutStrLn stderr str
    hSetSGR stderr []

errorRed :: String -> IO ()
errorRed str = do
    hSetSGR stderr [SetColor Foreground Vivid Red]
    hPutStrLn stderr str
    hSetSGR stderr []
