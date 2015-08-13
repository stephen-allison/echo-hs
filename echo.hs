import Network            --for sockets
import Control.Concurrent --for forkIO
import System.IO

-- program to try out haskell for a
-- very simple socket server.

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 5555
  acceptLoop sock


acceptLoop sock = do
  (h,_,_) <- accept sock

  -- forkIO in in Control.Concurrent
  -- creates a lightweight thread
  -- cf. forkOS
  -- takes an IO () computation
  forkIO $ serve h
  acceptLoop sock


serve :: Handle -> IO ()
serve h = do
  msg <- hGetLine h
  hPutStrLn h msg
  hFlush h
  serve h
