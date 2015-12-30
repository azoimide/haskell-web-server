module Resources (
    responseFromRequest
) where

import HTTP

import Data.List.Split(splitOn)
--import System.Directory(doesFileExist)

baseDirectory :: FilePath
baseDirectory = "resources/testsite"

getFile :: String -> FilePath
getFile "/" = getFile "/index.html"
getFile s = s

getContentType :: String -> String
getContentType f
    | su == "html" = "text/" ++ su
    | su == "css" = "text/" ++ su
    | su == "js" = "*/*"
    | otherwise = "*/*"
    where su = last (splitOn "." f)

responseFromRequest :: HTTPRequest -> IO HTTPResponse
responseFromRequest req = fmap (addHeaderToProtocol "Content-Type" (getContentType f)) $ fmap (responseProtocol 200) (readFile (baseDirectory ++ f))
--    | (doesFileExist f) = fmap (addHeaderToProtocol "Content-Type" (getContentType f)) $ fmap (responseProtocol 200) (readFile (baseDirectory ++ f))
--    | otherwise = fmap (responseProtocol 404) (read "404 - Not Found")
    where f = (getFile (path (uri req)))