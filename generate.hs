import Distribution.PackDeps
import System.Environment (getArgs)
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (unless)
import System.IO (hPutStrLn, stderr)
import Distribution.Text (display)
import Distribution.Package (Dependency (..), PackageName (..))

data Package = Package
    { name :: String
    , version :: String
    }
    deriving (Eq, Ord)

type M = RWST Newest (Set.Set Package) (Set.Set String) IO

bootlibs :: Set.Set String
bootlibs = Set.fromList $ words
    "containers ghc-binary ghc-prim Cabal base old-locale old-time time random syb template-haskell filepath haskell98 unix Win32 bytestring deepseq array network process directory" -- FIXME

main :: IO ()
main = do
    toProcess <- getArgs
    newest <- loadNewest
    ((), _, packages) <- runRWST (mapM_ process toProcess) newest bootlibs
    mapM_ (\(Package n v) -> putStrLn $ unwords [n, v]) $ Set.toList packages

process :: String -> M ()
process package = do
    processed <- get
    unless (package `Set.member` processed) $ do
        put $ Set.insert package processed
        newest <- ask
        case Map.lookup package newest of
            Nothing -> liftIO $ hPutStrLn stderr $ "Warning: Unknown package: " ++ package
            Just (PackInfo version mdi _) -> do
                tell $ Set.singleton $ Package package $ display version
                case mdi of
                    Nothing -> liftIO $ hPutStrLn stderr $ "Warning: No information on: " ++ package
                    Just (DescInfo { diDeps = deps }) ->
                        mapM_ (\(Dependency (PackageName n) _) -> process n) deps
