module Resources (
    responseFromRequest
) where

import HTTP

import Data.List.Split(splitOn)
import System.Directory(doesFileExist)

import Data.ByteString.Lazy as BS(readFile)
import Data.ByteString.Lazy.Char8 as BS(pack)

baseDirectory :: FilePath
baseDirectory = "resources/testsite"

-- Here aliases can be specified
getFile :: String -> FilePath
getFile s  
    | s == "/" || s == [] = getFile "/index.html"
    | otherwise = s

getContentType :: String -> String
getContentType f

    -- text
    | su == "rtx" = "text/richtext"
    | 
        su == "html" || 
        su == "htm" ||
        su == "stm" = "text/html"
    | 
        su == "txt" || 
        su == "bas" ||
        su == "c" ||
        su == "h" = "text/plain"
    | 
        su == "css" ||
        su == "js" = "text/" ++ su

    -- image
    | su == "svg" = "image/svg+xml"
    | su == "ico" = "image/x-icon"
    | 
        su == "jpe" ||
        su == "jpeg" ||
        su == "jpg" = "image/jpeg"
    | 
        su == "tif" ||
        su == "tiff" = "image/tiff"
    | 
        su == "bmp" ||
        su == "png" ||
        su == "gif" = "image/" ++ su

    -- audio
    | su == "mp3" = "audio/mpeg"
    | su == "wav" = "audio/x-wav"
    | 
        su == "flac" ||
        su == "ogg" = "audio/" ++ su

    -- video
    | 
        su == "mp2" ||
        su == "mpa" ||
        su == "mpe" ||
        su == "mpeg" ||
        su == "mpg" ||
        su == "mpv2" = "video/mpeg"
    | 
        su == "mov" ||
        su == "qt" = "video/quicktime"

    | otherwise = "*/*"
    where su = last (splitOn "." f)

responseFromRequest :: HTTPRequest -> IO HTTPResponse
responseFromRequest req = do
    exists <- doesFileExist filename
    if exists 
        then do
            content <- (BS.readFile filename)
            return $ addHeaderToProtocol "Content-Type" contenttype (responseProtocol 200 content)
        else
            return $ responseProtocol 404 (pack "404 - Not Found")
    where 
        filename = baseDirectory ++ (getFile (path (uri req)))
        contenttype = getContentType filename