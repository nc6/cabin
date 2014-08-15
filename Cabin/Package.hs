{-# LANGUAGE DeriveGeneric #-}
module Cabin.Package where

import Cabin.Internal
import Control.Monad (liftM, filterM)
import Control.Exception (IOException, catch)

import Data.Binary
import Data.List (delete, find)
import Data.Monoid

import GHC.Generics (Generic)

import System.Directory (doesFileExist, getDirectoryContents, removeFile)
import System.FilePath
import System.IO (stderr, hPutStrLn)
import System.Process (readProcess)

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

data CabinInfo = CabinInfo {
    cPackages :: [String]
  , cBinaries :: [String]
} deriving (Generic, Show)

instance Binary CabinInfo

data Cabin = Cabin String CabinInfo deriving (Show, Generic)

instance Binary Cabin

-- | Get the name of a cabin
cabinName :: Cabin -> String
cabinName (Cabin name _) = name

data Status = Loaded | Unloaded deriving (Eq, Show)

data CabinDB = CabinDB [Cabin]
  deriving (Generic)

instance Binary CabinDB

instance Monoid CabinDB where
  mempty = CabinDB []
  (CabinDB a) `mappend` (CabinDB b) = CabinDB $ a `mappend` b

--------------------------------------------------------------------------------
-- Working with the package DB
--------------------------------------------------------------------------------

listCabins :: CabinDB -> [Cabin]
listCabins (CabinDB cabs) = cabs

findCabin :: (Cabin -> Bool) -> CabinDB -> Maybe Cabin
findCabin f (CabinDB cabs) = find f cabs

findCabinByName :: String -> CabinDB -> Maybe Cabin
findCabinByName name = findCabin (\(Cabin name' _) -> name == name')

readCabinDB :: IO CabinDB
readCabinDB = catch
  (getCabinDBPath >>= \path -> (decodeFile path :: IO CabinDB))
  ((\_ -> do
    return mempty
  ) :: (IOException -> IO CabinDB))

writeCabinDB :: CabinDB
               -> IO ()
writeCabinDB db = catch
  (getCabinDBPath >>= \path -> encodeFile path db)
  (\e -> do
    let err = show (e :: IOException)
    hPutStrLn stderr ("Error: Couldn't write to cabin DB:\n\t" ++ err)
  )

--------------------------------------------------------------------------------
-- Querying packages
--------------------------------------------------------------------------------

infoCabin :: FilePath -> IO Cabin
infoCabin path = do
    cabal <- getCabalPath
    pkgs <- fmap lines $ readProcess cabal [
        "--sandbox=" ++ sandboxPath
      , "sandbox"
      , "hc-pkg"
      , "--"
      , "list"
      ] ""
    bins <- getDirectoryContents binDir
          |>  liftM (binDir </>)
          >>= filterM doesFileExist
    return $ Cabin name $ CabinInfo pkgs bins
  where
    name = takeBaseName path
    sandboxPath = path </> "cabal.sandbox.config"
    binDir = cabinBinaryDir path

--------------------------------------------------------------------------------
-- Working with the status DB
--------------------------------------------------------------------------------

loadedCabins :: IO [String]
loadedCabins = do
  loadedDir <- getLoadedPath
  loaded <- (getDirectoryContents loadedDir
            |> liftM (loadedDir </>)
            >>= filterM doesFileExist)
            |> liftM takeFileName
  return loaded

cabinStatus :: Cabin -> IO Status
cabinStatus (Cabin name _) = do
  loadedDir <- getLoadedPath
  exists <- doesFileExist $ loadedDir </> name
  return $ case exists of
    True -> Loaded
    False -> Unloaded

writeCabinStatus :: Cabin -> Status -> IO ()
writeCabinStatus c@(Cabin name _) stat = do
  oldStatus <- cabinStatus c
  case (stat, oldStatus) of
    (Loaded, Unloaded) -> do -- Mark as loaded
      loadedDir <- getLoadedPath
      writeFile (loadedDir </> name) ""
    (Unloaded, Loaded) -> do -- Mark as unloaded
      loadedDir <- getLoadedPath
      removeFile (loadedDir </> name)

