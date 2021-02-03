module Utils where

import Control.Monad
import GHC.IO.Encoding ( utf8 )
import System.IO
import System.Process
import Pipes

initHandles :: IO (Handle, Handle, Handle, Handle)
initHandles = do
    { (recvByRepl, sendToRepl)   <- createPipe
    ; (recvFromRepl, sendByRepl) <- createPipe
    ; mapM_ (`hSetBuffering` NoBuffering) [stdout, recvByRepl, sendToRepl, recvFromRepl, sendByRepl]
    ; mapM_ (`hSetEncoding` utf8) [recvByRepl, sendToRepl, recvFromRepl, sendByRepl]
    ; return (recvByRepl, sendToRepl, recvFromRepl, sendByRepl)
    }

mkCreateProcess :: String -> (Handle, Handle) -> CreateProcess
mkCreateProcess cmdln (rh, wh)
    = (shell cmdln) { std_in  = UseHandle rh
                    , std_out = UseHandle wh
                    , std_err = UseHandle wh
                    }


toHandle :: Handle -> Pipe String () IO ()
toHandle h = go
    where
        go = do
            { str <- await
            ; liftIO $ hPutStrLn h str
            ; yield ()
            ; go
            }

fromHandle :: Handle -> String -> Pipe () String IO ()
fromHandle h prompt = go
    where
        go = do
            { _ <- await
            ; eof <- liftIO $ hIsEOF h
            ; unless eof $ do
                { str <- liftIO $ hGetUntil h ('\n' : prompt)
                ; yield str
                ; go
                }
            }

output :: Consumer String IO ()
output = do
    { str <- await
    ; liftIO (putStr str)
    ; output
    }

takeUntil' :: Functor m => (a -> Bool) -> Pipe a a m ()
takeUntil' predicate = go
    where
        go = do
            { a <- await
            ; if predicate a
              then do
                  { yield a
                  ; return ()
                  }
              else do
                  { yield a
                  ; go
                  }
            }

hGetUntil :: Handle -> String -> IO String
hGetUntil h str = do
    { eof <- hIsEOF h
    ; if eof then return ""
      else do
          { c <- hGetChar h
          ; if c == head str then (c :) <$> getStr (tail str)
            else (c :) <$> hGetUntil h str
          }
    }
    where
        getStr []     = return ""
        getStr (c:cs) = do
            { eof <- hIsEOF h
            ; if eof then return ""
              else do
                  { c' <- hGetChar h
                  ; if c == c' then (c' :) <$> getStr cs
                    else (c' :) <$> hGetUntil h str
                  }
            }
