module Main where

import Control.Arrow
import System.Environment
import Text.Printf

main :: IO ()
main = do
    { ffs <- getArgs
    ; case ffs of
        []  -> readFile "src/Lib.hs" >>= putStr . numbering
        f:_ -> readFile f            >>= putStr . numbering
    }

numbering :: String -> String
numbering = lines >>> zipWith comb [1..] >>> unlines

comb :: Int -> String -> String
comb = printf "%6d  %s"
