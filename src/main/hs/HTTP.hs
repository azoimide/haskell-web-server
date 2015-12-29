module HTTP (
	HTTPProtocol,
	responseProtocol,
	htmlResponseProtocol
) where

import Text.Printf(printf)

responseProtocol :: Int -> String -> HTTPProtocol
responseProtocol status content = HTTPProtocol 1 1 status [Header "Content-Length" (show (length content))] content

addHeaderToProtocol :: HTTPProtocol -> Header -> HTTPProtocol
addHeaderToProtocol (HTTPProtocol v sv s oh content) h = (HTTPProtocol v sv s (oh ++ [h]) content)

htmlResponseProtocol status content = addHeaderToProtocol (responseProtocol status content) (Header "Content-Type" "text/html")

data Header = Header {
	identity :: String,
	value :: String
}

instance Show Header where
	show h = (identity h) ++ ": " ++ (value h) ++ "\r\n"

showHeaders :: [Header] -> String
showHeaders [] = []
showHeaders (h:hs) = ((show h) ++ (showHeaders hs))

-- should be unsigned
data HTTPProtocol = HTTPProtocol {
	version :: Int,
	subVersion :: Int,
	status :: Int,
	headers :: [Header],
	content :: String
}

instance Show HTTPProtocol where
	show p = printf
		"HTTP/%d.%d %d %s\r\n%s\r\n%s" 
		(version p) (subVersion p) (status p) (statusString (status p))
		(showHeaders (headers p))
		(content p)
	--show p = 
	--	"HTTP/" ++ (show (version p)) ++ "." ++ (show (subVersion p)) ++ " " ++ 
	--	(show (status p)) ++ " " ++ (statusString (status p)) ++ "\r\n" ++
	--	(showHeaders (headers p)) ++ 
	--	"\r\n" ++
	--	(content p)




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
