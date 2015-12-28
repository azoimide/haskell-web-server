module Main where 

import Network.Socket
import HTTP


defaultResponseProtocol = HTTPProtocol 1 1 200 "text/html" 3 "hej"

defaultResponse :: String
defaultResponse = (show defaultResponseProtocol)

main :: IO ()
main = do
    sock <- serverSocket 8080
    acceptLoop sock

serverSocket :: PortNumber -> IO Socket
serverSocket port = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    return sock

acceptLoop :: Socket -> IO ()
acceptLoop sock = do
    (cliSock, _) <- accept sock
    _ <- send cliSock defaultResponse
    acceptLoop sock

