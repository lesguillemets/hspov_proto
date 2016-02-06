module Data.Povray where
import Data.Povray.Types

include :: String -> IO ()
include = putStrLn
