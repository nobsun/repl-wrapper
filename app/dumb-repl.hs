module Main where

import Data.Char ( isDigit )
import Data.List ( isPrefixOf )
import System.IO
    ( hSetBuffering,
      isEOF,
      stderr,
      stdout,
      hPutStrLn,
      BufferMode(NoBuffering) )

main :: IO ()
main = do
  { hSetBuffering stdout NoBuffering
  ; hSetBuffering stderr NoBuffering
  ; repl
  }

repl :: IO ()
repl = do
  { putStr prompt
  ; eof <- isEOF
  ; if eof then putStrLn "\nLeaving fizzbuzz"
    else do
      { inp <- getLine
      ; if ":q" `isPrefixOf` inp then putStrLn "\nLeaving fizzbuzz"
        else do 
          { if all isDigit inp then fizzbuzz (read inp)
            else hPutStrLn stderr $ "Not a number: " ++ inp
          ; repl
          }
      }
  }

fizzbuzz :: Int -> IO ()
fizzbuzz n
  | 0 == n `mod` 15 = putStrLn "FizzBuzz"
  | 0 == n `mod`  5 = putStrLn "Buzz"
  | 0 == n `mod`  3 = putStrLn "Fizz"
  | otherwise       = print n

prompt :: String
prompt = ">>> "
