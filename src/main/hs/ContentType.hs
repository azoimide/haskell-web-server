module ContentType (
    getContentType
) where

import Data.List.Split(splitOn)

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
