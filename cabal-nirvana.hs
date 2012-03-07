import System.Environment (getArgs)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.Char (isSpace)
import Data.List (isPrefixOf, foldl')
import System.Exit (exitWith, ExitCode (ExitSuccess))
import System.Cmd (rawSystem)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import qualified Data.ByteString.Lazy as L
import qualified Codec.Archive.Tar as Tar
import Control.Exception (throwIO)
import qualified Data.Map as Map
import Control.Monad (unless)
import Data.Maybe (fromMaybe)

indexFile :: IO FilePath
indexFile = do
    dir <- getAppUserDataDirectory "cabal"
    return $ dir ++ "/packages/hackage.haskell.org/00-index.tar"

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

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            ec <- rawSystem "cabal" ["update"]
            unless (ec == ExitSuccess) $ exitWith ec
            fetch Nothing
            start Nothing
        ["fetch"] -> fetch Nothing
        ["fetch", url] -> fetch $ Just url
        ["start"] -> start Nothing
        ["start", nfp] -> start $ Just nfp
        ["test"] -> test Nothing
        ["test", fp] -> test $ Just fp
        _ -> do
            putStrLn "Available commands:"
            putStrLn "    cabal-nirvana"
            putStrLn "    cabal-nirvana fetch [URL]"
            putStrLn "    cabal-nirvana start [package file]"
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

fetch :: Maybe String -> IO ()
fetch murl = do
    str <- simpleHTTP (getRequest url) >>= getResponseBody
    nirvanaFile >>= flip writeFile str
  where
    url = fromMaybe "http://www.snoyman.com/cabal-nirvana/yesod" murl

start :: Maybe String -> IO ()
start mnfp = do
    nfp <- maybe nirvanaFile return mnfp
    packages <- readPackages nfp
    let packMap = Map.fromList $ map (\(Package n v) -> (n, v)) packages
    start' packMap
  where
    start' packMap = do
        ifp <- indexFile
        entries <- Tar.read <$> L.readFile ifp
        go id entries >>= L.writeFile ifp . Tar.write
      where
        go front Tar.Done = return $ front []
        go _ (Tar.Fail e) = throwIO e
        go front (Tar.Next e es) =
            go front' es
          where
            fp = Tar.entryPath e
            (p, fp') = break (== '/') fp
            v = takeWhile (/= '/') $ drop 1 fp'
            front' =
                if toRemove p v
                    then front
                    else front . (e:)

        toRemove _ "" = False
        toRemove p v =
            case Map.lookup p packMap of
                Nothing -> False
                Just v' -> v /= v'

test :: Maybe FilePath -> IO ()
test mfp = do
    fp <- maybe nirvanaFile return mfp
    packages <- readPackages fp
    let ws = map (\(Package n v) -> concat [n, "-", v]) packages
    putStrLn $ "cabal-dev install " ++ unwords ws
    ec <- rawSystem "cabal-dev" $ "install" : ws
    exitWith ec
