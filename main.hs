module Main where

import Cabin.Package
import Cabin.Internal
import Control.Monad (filterM, liftM)

import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, (<>))

import Options.Applicative
import Options.Applicative.Types (Parser(NilP))

import System.Directory
import System.Environment (lookupEnv)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath
import System.Posix.Files (createSymbolicLink)
import System.Process

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

data InstallOpts = InstallOpts {
    ioName :: Maybe String
  , ioPackages :: [String]
}

installOpts :: Parser InstallOpts
installOpts = InstallOpts
  <$> optional (strOption (
         long "name"
      <> short 'n'
      <> metavar "NAME"
      <> help ("Name for the new cabin. If not provided, the first package "
          ++ "name is used.")
      ))
  <*> some (argument str (metavar "PACKAGES..."))

data LoadOpts = LoadOpts {
  loNames :: [String]
}

loadOpts :: Parser LoadOpts
loadOpts = LoadOpts
  <$> some (argument str (metavar "CABINS..."))

data ListOpts = ListOpts {
  loOnlyActive :: Bool
}

listOpts :: Parser ListOpts
listOpts = ListOpts
  <$> switch (   long "active"
              <> short 'a'
              <> help "Show only active cabins."
             )

emptyOpts :: Parser ()
emptyOpts = NilP $ Just ()

data Command = Install InstallOpts
             | List ListOpts
             | Load LoadOpts
             | Unload LoadOpts
             | Reindex ()

commandParser :: Parser Command
commandParser = subparser (
      command "install" (info (fmap Install installOpts)
        (progDesc "Install a program into a new cabin."))
   <> command "list" (info (fmap List listOpts) (
        progDesc "List available cabins."))
   <> command "load" (info (fmap Load loadOpts) (
        progDesc "Load an installed cabin."))
   <> command "unload" (info (fmap Unload loadOpts) (
        progDesc "Unload a loaded cabin."))
   <> command "reindex" (info (fmap Reindex emptyOpts) (
        progDesc "Rebuild the cabin DB."))
  )

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = execParser opts >>= run where
  opts = info (helper <*> commandParser) (
       fullDesc
    <> progDesc "Cabin - cabal binary tool."
    <> header "Cabin."
    )

run :: Command -> IO ()
run (Install opts) = install opts
run (List opts) = list opts
run (Reindex _) = reindex
run (Load opts) = load opts
run (Unload opts) = unload opts
--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

install :: InstallOpts -> IO ()
install opts = do
  prepareEnvironment
  let pkgs = ioPackages opts
      name = fromMaybe (head pkgs) $ ioName opts
  cabal <- getCabalPath
  cabinPath <- fmap (</> name) getCabinPath
  cabinDB <- readCabinDB
  -- Create directory, sandbox init, cabal init
  createDirectoryIfMissing True cabinPath
  let sboxInitProc = (proc cabal ["sandbox","init"]) {
        cwd = Just cabinPath
      }
      cabalInstallProc = (proc cabal ("install" : pkgs)) {
        cwd = Just cabinPath
      }
  ec <- cmdSeq [sboxInitProc, cabalInstallProc]
  case ec of
    ExitSuccess -> do
      cabin <- infoCabin cabinPath
      let cabinDB' = cabinDB <> (CabinDB [cabin])
      writeCabinDB cabinDB'
    ExitFailure _ -> removeDirectoryRecursive cabinPath
  exitWith ec

list :: ListOpts -> IO ()
list opts = do
    dataDir <- getCabinPath
    createDirectoryIfMissing True dataDir
    cabs <- readCabinDB
    cabStats <- mapM (\a -> cabinStatus a >>= \b -> return (a,b)) $ listCabins cabs
    mapM_ (putStrLn . showCab) cabStats
  where showCab ((Cabin name _, status)) = name ++ " (" ++ show status ++ ")"

load :: LoadOpts -> IO ()
load opts = do
    prepareEnvironment
    cdb <- readCabinDB
    binPath <- getBinaryPath
    mapM_ (go cdb binPath) $ loNames opts
  where
    go cdb binPath name =
      let cabin = findCabinByName name cdb
          linkPath a = binPath </> (takeFileName a)
      in case cabin of
        Nothing -> putStrLn $ "No cabin "++name++" found!"
        Just c@(Cabin _ (CabinInfo _ bins)) -> do
          stat <- cabinStatus c
          case stat of
            Loaded -> return ()
            Unloaded -> do
              mapM_ (\a -> createSymbolicLink a (linkPath a)) bins
              writeCabinStatus c Loaded

unload :: LoadOpts -> IO ()
unload opts = do
    prepareEnvironment
    cdb <- readCabinDB
    binPath <- getBinaryPath
    mapM_ (go cdb binPath) $ loNames opts
  where
    go cdb binPath name =
      let cabin = findCabinByName name cdb
          linkPath a = binPath </> (takeFileName a)
      in case cabin of
        Nothing -> putStrLn $ "No cabin "++name++" found!"
        Just c@(Cabin _ (CabinInfo _ bins)) -> do
          stat <- cabinStatus c
          case stat of
            Unloaded -> return ()
            Loaded -> do
              mapM_ (\a -> removeFile $ linkPath a) bins
              writeCabinStatus c Unloaded

reindex :: IO ()
reindex = do
  prepareEnvironment
  cp <- getCabinPath
  putativeCabins <- liftM (filter (
                            \a -> notElem (takeFileName a) [".", ".."]
                          )) $
                    getDirectoryContents cp
                  |> liftM (cp </>)
                  >>= filterM doesDirectoryExist
  putStrLn . show $ putativeCabins
  cabins <- mapM infoCabin putativeCabins
  writeCabinDB $ CabinDB cabins

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

prepareEnvironment :: IO ()
prepareEnvironment = do
  getCabinPath >>= createDirectoryIfMissing True
  getBinaryPath >>= createDirectoryIfMissing True
  getLoadedPath >>= createDirectoryIfMissing True
