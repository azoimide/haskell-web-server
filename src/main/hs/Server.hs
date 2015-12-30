module Main where 

import Network.Socket

import HTTP
import Resources

main :: IO ()
main = do
    sock <- serverSocket 8083
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
    request <- recv cliSock 4096
    putStrLn $ show cliSock

    putStrLn request

    response <- fmap show $ (responseFromRequest (parseHTTPRequest (request)))
    --putStrLn response
    
    _ <- send cliSock response
    close cliSock
    acceptLoop sock

