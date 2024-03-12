module Main (main) where

import Lib

main :: IO ()
main = do
    mapM_ test cases
