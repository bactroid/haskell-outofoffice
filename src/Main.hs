module Main where

import DateArgs
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case makeDateMap (parseArgs args) of
    Nothing    -> putStrLn "Please enter at least one date."
    (Just drm) -> putStrLn . genMessage $ drm
