module Settings (
    acceptPortNumber,

    baseDirectory,
    getFileFromAlias
) where


import Network.Socket(PortNumber)

acceptPortNumber :: PortNumber
acceptPortNumber = 8080


-- NOTE: files start from "/", therefore the directory wont end with "/"

baseDirectory :: FilePath
baseDirectory = "resources/testsite"

-- Here aliases can be specified
-- TODO: maybe should be recursive?
getFileFromAlias :: String -> FilePath
getFileFromAlias s  
    | s == "/" = "/index.html"
    | otherwise = s
