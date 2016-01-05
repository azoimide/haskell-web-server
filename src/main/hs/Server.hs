module Main where 

import Network.Socket
import Network.Socket.ByteString.Lazy as BS(send)

import HTTP
import Resources

import Settings(acceptPortNumber)

main :: IO ()
main = do
    sock <- serverSocket acceptPortNumber
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

    --response <- fmap show (responseFromRequest (parseHTTPRequest (request)))
    
    --_ <- send cliSock response
    response <- fmap toByteString (responseFromRequest (parseHTTPRequest (request)))
    
    _ <- BS.send cliSock response
    close cliSock
    acceptLoop sock

