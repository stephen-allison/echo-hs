import Network            --for sockets
import Control.Concurrent --for forkIO
import System.IO

-- program to try out haskell for a
-- very simple socket server.

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 5555
  acceptLoop sock echo


acceptLoop sock server = do
  (h,_,_) <- accept sock

  -- forkIO in in Control.Concurrent
  -- creates a lightweight thread
  -- cf. forkOS
  -- takes an IO () computation
  forkIO $ server h
  acceptLoop sock server


echo :: Handle -> IO ()
echo h = do
  msg <- hGetLine h
  hPutStrLn h msg
  hFlush h
  echo h
