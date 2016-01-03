module Resources (
    responseFromRequest
) where

import HTTP

import Data.List.Split(splitOn)
import System.Directory(doesFileExist)

baseDirectory :: FilePath
baseDirectory = "resources/testsite"

getFile :: String -> FilePath
getFile s  
    | s == "/" || s == [] = getFile "/index.html"
    | otherwise = s

getContentType :: String -> String
getContentType f
    | su == "html" || 
        su == "htm" ||
        su == "stm"
        = "text/html"
    | su == "txt" || 
        su == "bas" ||
        su == "c" ||
        su == "h"
        = "text/plain"
    | su == "css" ||
        su == "js" 
        = "text/" ++ su
    | su == "rtx" = "text/richtext"

    | su == "bmp" ||
        su == "png" ||
        su == "gif"
        = "image/" ++ su
    | su == "jpe" ||
        su == "jpeg" ||
        su == "jpg"
        = "image/jpeg"
    | su == "svg" = "image/svg+xml"
    | su == "tif" ||
        su == "tiff"
        = "image/tiff"
    | su == "ico" = "image/x-icon"

    | su == "mp3" = "audio/mpeg"
    | su == "wav" = "audio/x-wav"
    | su == "flac" ||
        su == "ogg"
        = "audio/" ++ su

    | su == "mp2" ||
        su == "mpa" ||
        su == "mpe" ||
        su == "mpeg" ||
        su == "mpg" ||
        su == "mpv2"
        = "video/mpeg"
    | su == "mov" ||
        su == "qt"
        = "video/quicktime"

    | otherwise = "*/*"
    where su = last (splitOn "." f)

responseFromRequest :: HTTPRequest -> IO HTTPResponse
responseFromRequest req = do
    exists <- doesFileExist f
    if exists 
        then
            fmap (addHeaderToProtocol "Content-Type" (getContentType f)) $ fmap (responseProtocol 200) (readFile f)
        else
            return (responseProtocol 404 "404 - Not Found")
    where f = (baseDirectory ++ (getFile (path (uri req))))