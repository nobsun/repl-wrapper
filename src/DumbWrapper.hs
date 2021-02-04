module DumbWrapper where

import Control.Monad
import Pipes
import System.IO
import System.Process

import Utilities

wrapper :: FilePath -> IO ()
wrapper _ = do
    { (recvByRepl, sendToRepl)   <- createPipe
    ; (recvFromRepl, sendByRepl) <- createPipe
    ; hSetBuffering sendToRepl NoBuffering
    ; hSetBuffering sendByRepl NoBuffering
    ; hSetBuffering stdout NoBuffering
    ; let cp = (shell "dumb-repl") 
                { std_in = UseHandle recvByRepl
                , std_out = UseHandle sendByRepl
                , std_err = UseHandle sendByRepl
                }
    ; cl <- createProcess cp
    ; hGetUntil recvFromRepl prompt >>= putStr
    ; runEffect (inputLn >-> takeUntil' isQuitCmd 
                         >-> toHandle sendToRepl 
                         >-> fromHandle recvFromRepl prompt
                         >-> output)
    ; cleanupProcess cl
    }

inputLn :: Producer String IO ()
inputLn = do
    { eof <- lift isEOF
    ; unless eof $ do
        { str <- lift getLine
        ; case words str of
              [cmd] | cmd `elem` [":r", ":reload"] -> yield ":e"
              _                                    -> yield str
        ; inputLn
        }
    }

isQuitCmd :: String -> Bool
isQuitCmd str = case words str of
    [":q"]    -> True
    [":quit"] -> True
    _         -> False

prompt :: String
prompt = ">>> "
