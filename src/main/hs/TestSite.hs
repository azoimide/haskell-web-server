module TestSite (
	testSiteRespons
) where

import HTTP

baseDirectory = "resources/testsite/"


--rr :: String -> String
--rr path = do
--	s <- (readFile path)
--    --putStr s  
--	return $s

testSiteRespons :: String
testSiteRespons = (show (htmlResponseProtocol 200 ($ (readFile (baseDirectory ++ "index.html")))))
