module Main where 

import Network.Socket

import HTTP
import TestSite

defaultResponse :: String
defaultResponse = (show (htmlResponseProtocol 200 "Default response"))

main :: IO ()
main = do
    sock <- serverSocket 8081
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

