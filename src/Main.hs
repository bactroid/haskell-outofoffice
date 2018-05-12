module Main where

import DateArgs
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  -- let dt = parseArgs args
  -- case dt of
  --   (DateRange Nothing _) -> putStrLn "Please enter a valid start date."
  --   (DateRange (Just start) Nothing) -> putStrLn $ "Single Day: " ++ show start
  --   (DateRange (Just start) (Just end)) ->
      -- putStrLn $ "From " ++ show start ++ " to " ++ show end
  -- print $ makeDateMap (parseArgs args)
  case makeDateMap (parseArgs args) of
    Nothing -> putStrLn "Please enter at least one date."
    (Just drm) -> putStrLn . genMessage $ drm
