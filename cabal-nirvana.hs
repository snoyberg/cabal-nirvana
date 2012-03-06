import System.Environment (getArgs)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.Char (isSpace)
import Data.List (isPrefixOf, foldl')
import System.Exit (exitWith)
import System.Cmd (rawSystem)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

configFile :: IO FilePath
configFile = do
    dir <- getAppUserDataDirectory "cabal"
    return $ dir ++ "/config"

nirvanaFile :: IO FilePath
nirvanaFile = do
    dir <- getAppUserDataDirectory "cabal-nirvana"
    createDirectoryIfMissing True dir
    return $ dir ++ "/default"

data Package = Package
    { name :: String
    , version :: String
    }

readPackages :: FilePath -> IO [Package]
readPackages fp =
    readFile fp >>= (catMaybes <$>) . mapM parse . lines
  where
    parse s
        | all isSpace s = return Nothing
        | "--" `isPrefixOf` s = return Nothing
    parse s =
        case words s of
            [a, b] -> return $ Just $ Package a b
            _ -> error $ "Invalid nirvana line: " ++ show s

readConfigFile :: IO [String]
readConfigFile = do
    ls <- lines <$> (configFile >>= readFile)
    -- force evaluation
    if length ls > 0
        then return ls
        else return []

writeConfigFile :: [String] -> IO ()
writeConfigFile ss = configFile >>= flip writeFile (unlines ss)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("fetch":rest) -> do
            url <-
                case rest of
                    [] -> return "http://www.snoyman.com/cabal-nirvana/yesod"
                    [x] -> return x
                    _ -> error $ "Usage: cabal-nirvana fetch [URL]"
            str <- simpleHTTP (getRequest url) >>= getResponseBody
            nirvanaFile >>= flip writeFile str
        ("start":rest) -> do
            fp <-
                case rest of
                    [] -> nirvanaFile
                    [x] -> return x
                    _ -> error $ "Usage: cabal-nirvana start [package file]"
            packages <- readPackages fp
            config <- readConfigFile
            writeConfigFile $ addNirvana packages $ removeNirvana config
        ["stop"] -> do
            config <- readConfigFile
            writeConfigFile $ removeNirvana config
        ("test":rest) -> do
            fp <-
                case rest of
                    [] -> nirvanaFile
                    [x] -> return x
                    _ -> error $ "Usage: cabal-nirvana test [package file]"
            packages <- readPackages fp
            let ws = map (\(Package n v) -> concat [n, "-", v]) packages
            putStrLn $ "cabal-dev install " ++ unwords ws
            ec <- rawSystem "cabal-dev" $ "install" : ws
            exitWith ec
        _ -> do
            putStrLn "Available commands:"
            putStrLn "    cabal-nirvana fetch [URL]"
            putStrLn "    cabal-nirvana start [package file]"
            putStrLn "    cabal-nirvana stop"
            putStrLn "    cabal-nirvana test [package file]"

beginLine, endLine :: String
beginLine = "-- cabal-nirvana begin"
endLine = "-- cabal-nirvana end"

removeNirvana :: [String] -> [String]
removeNirvana [] = []
removeNirvana (x:xs)
    | x == beginLine = removeNirvana $ drop 1 $ dropWhile (/= endLine) xs
    | otherwise = x : removeNirvana xs

addNirvana :: [Package] -> [String] -> [String]
addNirvana ps =
    (++ ss)
  where
    ss = beginLine : foldr (:) [endLine] (map toS ps)
    toS (Package n v) = concat
        [ "constraint: "
        , n
        , " == "
        , v
        ]
