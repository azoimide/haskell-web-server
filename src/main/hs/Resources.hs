module Resources (
    responseFromRequest
) where

import HTTP

import System.Directory(doesFileExist)

import Data.ByteString.Lazy as BS(readFile)
import Data.ByteString.Lazy.Char8 as BS(pack)

import Settings(baseDirectory, getFileFromAlias)
import ContentType

responseFromRequest :: HTTPRequest -> IO HTTPResponse
responseFromRequest req = do
    exists <- doesFileExist filename
    if exists 
        then do
            content <- (BS.readFile filename)
            return $ addHeaderToProtocol "Content-Type" (getContentType filename) (responseProtocol 200 content)
        else
            return $ responseProtocol 404 (pack "404 - Not Found")
    where 
        filename = baseDirectory ++ (getFileFromAlias (path (uri req)))