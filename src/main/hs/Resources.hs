module Resources (
    responseFromRequest
) where

import HTTP

import Data.List.Split(splitOn)

baseDirectory :: String
baseDirectory = "resources/testsite"

--testSiteResponse :: IO String
--testSiteResponse = fmap show $ fmap (htmlResponseProtocol 200) (readFile (baseDirectory ++ "index.html"))

getFile :: String -> String
getFile s
    | s == "/" = "/index.html"
    | otherwise = s

getContentType :: String -> String
getContentType f
    | su == "html" = "text/" ++ su
    | su == "css" = "text/" ++ su
    | su == "js" = "*/*"
    | otherwise = "*/*"
    where su = last (splitOn "." f)

responseFromRequest :: HTTPRequest -> IO HTTPResponse
responseFromRequest req = fmap (addHeaderToProtocol "Content-Type" (getContentType f)) $ fmap (responseProtocol 200) (readFile (baseDirectory ++ f))
    where f = (getFile (path (uri req)))