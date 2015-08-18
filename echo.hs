import Network            --for sockets
import Control.Concurrent --for forkIO
import Control.Monad
import System.IO

-- program to try out haskell for a
-- very simple socket server.

main = withSocketsDo $ do
  dataStore <- newEmptyMVar
  sock <- listenOn $ PortNumber 5555
  forkIO $ storeServer dataStore []
  acceptLoop sock (echo dataStore)


acceptLoop sock server = do
  (h,_,_) <- accept sock

  -- forkIO in in Control.Concurrent
  -- creates a lightweight thread
  -- cf. forkOS
  -- takes an IO () computation
  forkIO $ server h
  acceptLoop sock server


echo :: MVar (String) -> Handle -> IO ()
echo dataStore h = do
  msg <- hGetLine h
  putMVar dataStore msg
  hPutStrLn h msg
  hFlush h
  echo dataStore h

storeServer :: MVar (String) -> [String] -> IO ()
storeServer dataStore messages = do
  putStrLn $ "StoreServer: " ++ show messages
  msg <- takeMVar dataStore
  storeServer dataStore $ msg : messages
