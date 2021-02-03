module Main where

import System.Environment
import GhciWrapper ( wrapper )

main :: IO ()
main = do
    { aas <- getArgs 
    ; case aas of
        []  -> wrapper "src/Lib.hs"
        a:_ -> wrapper a
    }
