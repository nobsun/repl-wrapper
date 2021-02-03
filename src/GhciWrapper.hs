module GhciWrapper where

import Control.Monad
import GHC.IO.Encoding
import Pipes
import System.IO
import System.Process

import Utils

wrapper :: FilePath -> IO ()
wrapper fp = do
    { (recvByRepl, sendToRepl, recvFromRepl, sendByRepl) <- initHandles
    ; let cp = mkCreateProcess ("stack exec -- ghci " ++ fp) (recvByRepl, sendByRepl)
            --    (shell $ "stack exec -- ghci " ++ fp) 
            --     { std_in = UseHandle recvByRepl
            --     , std_out = UseHandle sendByRepl
            --     , std_err = UseHandle sendByRepl
            --     }
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
