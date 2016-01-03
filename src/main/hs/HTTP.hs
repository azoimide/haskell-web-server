module HTTP (
    HTTPResponse,
    responseProtocol,
    addHeaderToProtocol,
    htmlResponseProtocol,

    HTTPRequest,
    parseHTTPRequest
) where

import Data.List.Split(splitOn)
import Text.Printf(printf)

type Key = String
type Value = String

data Header = Header {
    key :: Key,
    value :: Value
}

-- to string
instance Show Header where
    show h = (key h) ++ ": " ++ (value h) ++ "\r\n"

showHeaders :: [Header] -> String
showHeaders [] = []
showHeaders (h:hs) = ((show h) ++ (showHeaders hs))


-- from string
parseHeader :: String -> Header
parseHeader s = Header (head ss) (last ss) -- this works even if ss only has one element
    where ss = splitOn ": " s -- should only be two elements in list

parseHeaders :: [String] -> [Header]
parseHeaders [] = []
parseHeaders ([]:hs) = (parseHeaders hs) -- do not add empty lines as headers
parseHeaders (h:hs) = (parseHeader h):(parseHeaders hs)






responseProtocol :: Int -> String -> HTTPResponse
responseProtocol s c = HTTPResponse 1 1 s [Header "Content-Length" (show (length c))] c

addHeaderToProtocol :: HTTPResponse -> Header -> HTTPResponse
addHeaderToProtocol (HTTPResponse v sv s oh c) h = (HTTPResponse v sv s (oh ++ [h]) c)

htmlResponseProtocol :: Int -> String -> HTTPResponse
htmlResponseProtocol s c = addHeaderToProtocol (responseProtocol s c) (Header "Content-Type" "text/html")


-- should be unsigned 
data HTTPResponse = HTTPResponse {
    resVersion :: Int,
    resSubVersion :: Int,
    status :: Int,
    resHeaders :: [Header],
    content :: String
}

instance Show HTTPResponse where
    show p = printf
        "HTTP/%d.%d %d %s\r\n%s\r\n%s" 
        (resVersion p) (resSubVersion p) (status p) (statusString (status p))
        (showHeaders (resHeaders p))
        (content p)
    --show p = 
    --  "HTTP/" ++ (show (version p)) ++ "." ++ (show (subVersion p)) ++ " " ++ 
    --  (show (status p)) ++ " " ++ (statusString (status p)) ++ "\r\n" ++
    --  (showHeaders (headers p)) ++ 
    --  "\r\n" ++
    --  (content p)



-- Human readable string
statusString :: Int -> String
statusString s
    -- HTTP/1.1

    -- Informational 1xx
    | s == 100 = "Continue"
    | s == 101 = "Switching Protocols"

    -- Successful 2xx
    | s == 200 = "OK"
    | s == 201 = "Created"
    | s == 202 = "Accepted"
    | s == 203 = "Non-Authoritative Information"
    | s == 204 = "No Content"
    | s == 205 = "Reset Content"
    | s == 206 = "Partial Content"

    -- Redirection 3xx
    | s == 300 = "Multiple Choices"
    | s == 301 = "Moved Permanently"
    | s == 302 = "Found"
    | s == 303 = "See Other"
    | s == 304 = "Not Modified"
    | s == 305 = "Use Proxy"
    -- 306 Unused
    | s == 307 = "Temporary Redirect"

    -- Client Error 4xx
    | s == 400 = "Bad Request"
    | s == 401 = "Unauthorized"
    | s == 402 = "Payment Required"
    | s == 403 = "Forbidden"
    | s == 404 = "Not Found"
    | s == 405 = "Method Not Allowed"
    | s == 406 = "Not Acceptable"
    | s == 407 = "Proxy Authentication Required"
    | s == 408 = "Request Timeout"
    | s == 409 = "Conflict"
    | s == 410 = "Gone"
    | s == 411 = "Length Required"
    | s == 412 = "Precondition Failed"
    | s == 413 = "Request Entity Too Large"
    | s == 414 = "Request-URI Too Long"
    | s == 415 = "Unsupported Media Type"
    | s == 416 = "Requested Range Not Satisfiable"
    | s == 417 = "Expectation Failed"

    -- Server Error 5xx
    | s == 500 = "Internal Server Error"
    | s == 501 = "Not Implemented"
    | s == 502 = "Bad Gateway"
    | s == 503 = "Service Unavailable"
    | s == 504 = "Gateway Timeout"
    | s == 505 = "HTTP Version Not Supported"

    | otherwise = "_NO_STATUS_STRING_"






data Method = GET | UNSUPPORTED

parseMethod :: String -> Method
parseMethod s
    | s == "GET" = GET
    | otherwise = UNSUPPORTED


-- TODO: Add more parts to URI
data URI = URI {
    path :: String
}

parseURI :: String -> URI
parseURI s = URI s


data HTTPRequest = HTTPRequest {
    method :: Method,
    uri :: URI,
    reqVersion :: Int,
    reqSubVersion :: Int,
    reqHeaders :: [Header]
}

parseHTTPRequest :: String -> HTTPRequest
parseHTTPRequest s = HTTPRequest (parseMethod (head fl)) (parseURI (head (tail fl))) (read (head vv) :: Int) (read (last vv) :: Int) (parseHeaders (tail ss))
    where 
        ss = splitOn "\r\n" s
        fl = splitOn " " (head ss)
        vv = splitOn "." (last (splitOn "/" (last fl)))

