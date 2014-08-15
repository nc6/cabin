module Cabin.Internal where

import Control.Applicative
import Data.Maybe (fromMaybe)

import System.Directory (findExecutable, getAppUserDataDirectory)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath
import System.IO
import System.Process

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

getDataDir :: IO FilePath
getDataDir = getAppUserDataDirectory "cabin"

-- | Location of cabins
getCabinPath :: IO FilePath
getCabinPath = fmap (</> "cabins") getDataDir

-- | Binary path (cabin binaries will be linked here)
getBinaryPath :: IO FilePath
getBinaryPath = fmap (</> "bin") getDataDir

-- | Package DB
getCabinDBPath :: IO FilePath
getCabinDBPath = fmap (</> "cabin.db") getDataDir

getLoadedPath :: IO FilePath
getLoadedPath = fmap (</> "loaded") getDataDir

cabinBinaryDir :: FilePath -> FilePath
cabinBinaryDir = (</> ".cabal-sandbox/bin")

getCabalPath :: IO FilePath
getCabalPath = findExecutable "cabal" >>= return . fromMaybe "/usr/bin/cabal"

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- | Executes a sequence of commands, where the execution of one command is
--   predicated upon the successful completion of the next.
cmdSeq :: [CreateProcess] -> IO ExitCode
cmdSeq [] = return ExitSuccess
cmdSeq (x : xs) = do
  x <- do
      (_,_,_,ph) <- createProcess x
      waitForProcess ph
  case x of
    ExitSuccess -> cmdSeq xs
    a -> return a

(|>) :: Functor f => f a -> (a -> b) -> f b
(|>) = flip (<$>)