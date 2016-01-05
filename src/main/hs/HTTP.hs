module HTTP (
    HTTPResponse,
    responseProtocol,
    addHeaderToProtocol,
    toByteString,

    HTTPRequest,
    parseHTTPRequest,
    path, 
    uri
) where

import Data.List.Split(splitOn)
import Text.Printf(printf)
import Data.ByteString.Lazy as BS(ByteString, length, append)
import Data.ByteString.Lazy.Char8(pack)

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





responseProtocol :: Int -> ByteString -> HTTPResponse
responseProtocol s c = HTTPResponse 1 1 s [Header "Content-Length" (show (BS.length c))] c

addHeaderToProtocol :: Key -> Value -> HTTPResponse -> HTTPResponse
addHeaderToProtocol k val (HTTPResponse v sv s oh c) = (HTTPResponse v sv s (oh ++ [Header k val]) c)


-- should be unsigned 
data HTTPResponse = HTTPResponse {
    resVersion :: Int,
    resSubVersion :: Int,
    status :: Int,
    resHeaders :: [Header],
    content :: ByteString
}

toByteString :: HTTPResponse -> ByteString
toByteString p = append 
        (pack (printf
        "HTTP/%d.%d %d %s\r\n%s\r\n" 
        (resVersion p) (resSubVersion p) (status p) (statusString (status p))
        (showHeaders (resHeaders p))))
        
        (content p)


--instance Show HTTPResponse where
--    show p = printf
--        "HTTP/%d.%d %d %s\r\n%s\r\n%s" 
--        (resVersion p) (resSubVersion p) (status p) (statusString (status p))
--        (showHeaders (resHeaders p))
--        (unpack (content p))
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
    | s == 100 = "CONTINUE"
    | s == 101 = "SWITCHING_PROTOCOLS"

    -- Successful 2xx
    | s == 200 = "OK"
    | s == 201 = "CREATED"
    | s == 202 = "ACCEPTED"
    | s == 203 = "NON-AUTHORITATIVE_INFORMATION"
    | s == 204 = "NO_CONTENT"
    | s == 205 = "RESET_CONTENT"
    | s == 206 = "PARTIAL_CONTENT"

    -- Redirection 3xx
    | s == 300 = "MULTIPLE_CHOICES"
    | s == 301 = "MOVED_PERMANENTLY"
    | s == 302 = "FOUND"
    | s == 303 = "SEE_OTHER"
    | s == 304 = "NOT_MODIFIED"
    | s == 305 = "USE_PROXY"
    -- 306 Unused
    | s == 307 = "TEMPORARY_REDIRECT"

    -- Client Error 4xx
    | s == 400 = "BAD_REQUEST"
    | s == 401 = "UNAUTHORIZED"
    | s == 402 = "PAYMENT_REQUIRED"
    | s == 403 = "FORBIDDEN"
    | s == 404 = "NOT_FOUND"
    | s == 405 = "METHOD_NOT_ALLOWED"
    | s == 406 = "NOT_ACCEPTABLE"
    | s == 407 = "PROXY_AUTHENTICATION_REQUIRED"
    | s == 408 = "REQUEST_TIMEOUT"
    | s == 409 = "CONFLICT"
    | s == 410 = "GONE"
    | s == 411 = "LENGTH_REQUIRED"
    | s == 412 = "PRECONDITION_FAILED"
    | s == 413 = "REQUEST_ENTITY_TOO_LARGE"
    | s == 414 = "REQUEST-URI_TOO_LONG"
    | s == 415 = "UNSUPPORTED_MEDIA_TYPE"
    | s == 416 = "REQUESTED_RANGE_NOT_SATISFIABLE"
    | s == 417 = "EXPECTATION_FAILED"

    -- Server Error 5xx
    | s == 500 = "INTERNAL_SERVER_ERROR"
    | s == 501 = "NOT_IMPLEMENTED"
    | s == 502 = "BAD_GATEWAY"
    | s == 503 = "SERVICE_UNAVAILABLE"
    | s == 504 = "GATEWAY_TIMEOUT"
    | s == 505 = "HTTP_VERSION_NOT_SUPPORTED"

    | otherwise = "_NO_STATUS_STRING_"


--  -- Informational 1xx
--  | s == 100 = "Continue"
--  | s == 101 = "Switching Protocols"
--
--  -- Successful 2xx
--  | s == 200 = "OK"
--  | s == 201 = "Created"
--  | s == 202 = "Accepted"
--  | s == 203 = "Non-Authoritative Information"
--  | s == 204 = "No Content"
--  | s == 205 = "Reset Content"
--  | s == 206 = "Partial Content"
--
--  -- Redirection 3xx
--  | s == 300 = "Multiple Choices"
--  | s == 301 = "Moved Permanently"
--  | s == 302 = "Found"
--  | s == 303 = "See Other"
--  | s == 304 = "Not Modified"
--  | s == 305 = "Use Proxy"
--  -- 306 Unused
--  | s == 307 = "Temporary Redirect"
--
--  -- Client Error 4xx
--  | s == 400 = "Bad Request"
--  | s == 401 = "Unauthorized"
--  | s == 402 = "Payment Required"
--  | s == 403 = "Forbidden"
--  | s == 404 = "Not Found"
--  | s == 405 = "Method Not Allowed"
--  | s == 406 = "Not Acceptable"
--  | s == 407 = "Proxy Authentication Required"
--  | s == 408 = "Request Timeout"
--  | s == 409 = "Conflict"
--  | s == 410 = "Gone"
--  | s == 411 = "Length Required"
--  | s == 412 = "Precondition Failed"
--  | s == 413 = "Request Entity Too Large"
--  | s == 414 = "Request-URI Too Long"
--  | s == 415 = "Unsupported Media Type"
--  | s == 416 = "Requested Range Not Satisfiable"
--  | s == 417 = "Expectation Failed"
--
--  -- Server Error 5xx
--  | s == 500 = "Internal Server Error"
--  | s == 501 = "Not Implemented"
--  | s == 502 = "Bad Gateway"
--  | s == 503 = "Service Unavailable"
--  | s == 504 = "Gateway Timeout"
--  | s == 505 = "HTTP Version Not Supported"






data Method = GET | UNSUPPORTED

parseMethod :: String -> Method
parseMethod s
    | s == "GET" = GET
    | otherwise = UNSUPPORTED

data URI = URI {
    scheme :: String,
    -- userinfo :: String,
    host :: String,
    port :: String, -- TODO: maybe change to number
    path :: String,
    query :: String,
    fragment :: String
} deriving (Show)  


-- example:
-- abc://username:password@example.com:123/path/data?key=value#fragid1
parseURI :: String -> URI
parseURI s 
    | Prelude.length s1 == 1 = parseURI ("://" ++ s)
    | Prelude.length s4 == 1 = parseURI (s ++ "?")
    | otherwise = URI (head s1) (head s3) (last s3) (head s4) (head s5) (last s5)
    where 
        s1 = splitOn "://" s
        s2 = splitOn "/" (last s1)
        s3 = splitOn ":" (head s2)
        s4 = splitOn "?" (drop (Prelude.length (head s2)) (last s1))
        s5 = splitOn "#" (last s4)


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

