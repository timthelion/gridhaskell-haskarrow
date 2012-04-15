{-GPLv3-}
import Control.Concurrent
import System.Exit



main = newEmptyMVar >>= f0x0 
f0x0  exit   = getChar  >>= \v0x0 ->f0x1  exit  v0x0 
f0x1  exit  p1  = return ('o' ) >>= \v0x1 ->f0x2 v0x1  exit  p1 v0x1 
f0x2 v0x1  exit  p1 p2  = return ((==) p1 p2 ) >>= \v0x2 ->f0x3 v0x1  exit  v0x2 
f0x3 v0x1  exit  (True) = f2x6 v0x1  exit 
f0x3 v0x1  exit  (False) = f_2x6 v0x1  exit 

f2x6 v0x1  exit   = return (v0x1  ) >>= \v2x6 ->f2x7 v0x1  exit  v2x6 
f2x7 v0x1  exit  p1  = putChar p1  >>= \v2x7 ->f2x8 v0x1  exit  
f2x8 v0x1  exit   = putMVar exit ExitSuccess >> exitWith ExitSuccess
f_2x6 v0x1  exit   = do char <- newEmptyMVar;f_2x7 v0x1  exit  char  
f_2x7 v0x1  exit  char   = do forkIO (f_4x9 v0x1  exit  char  );forkIO (f_1x11 v0x1  exit  char  );do signal <- takeMVar exit; putMVar exit signal; exitWith signal
f_4x9 v0x1  exit  char   = getChar  >>= \v_4x9 ->f_4x10 v0x1  exit  char  v_4x9 
f_4x10 v0x1  exit  char  p1  = putMVar char p1  >>= \v_4x10 ->f_4x11 v0x1  exit  char  
f_4x11 v0x1  exit  char   = return ()
f_1x11 v0x1  exit  char   = takeMVar char  >>= \v_1x11 ->f_1x12 v0x1  exit  char  v_1x11 
f_1x12 v0x1  exit  char  p1  = f0x1  exit  p1 