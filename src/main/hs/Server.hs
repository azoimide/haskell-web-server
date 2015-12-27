module Main where 

import Network.Socket

defaultResponse :: String
defaultResponse = "HTTP/1.0 200 OK\r\nContent-Length: 3\r\n\r\nhej"

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

