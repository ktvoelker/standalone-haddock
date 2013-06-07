import Options.Applicative
import Cabal.Simple
import Cabal.Haddock
import qualified Data.Set as Set
import Text.Printf
import System.Directory
import Data.Foldable (forM_)

import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.UserHooks
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Haddock (haddock, hscolour)
import Distribution.Simple.Utils hiding (info)
import Distribution.Verbosity
import Distribution.Text
import Distribution.InstalledPackageInfo

optParser =
  (,,)
    <$> many (strOption (long "package-db" <> metavar "DB-PATH"))
    <*> strOption (short 'o' <> metavar "OUTPUT-PATH")
    <*> many (argument str (metavar "PACKAGE-PATH"))

getPackageNames
  :: [FilePath]       -- ^ package directories
  -> IO [PackageName] -- ^ package names
getPackageNames = mapM $ \dir -> do
  cabalFile <- findPackageDesc dir
  desc <- readPackageDescription silent cabalFile
  let
    name = pkgName . package . packageDescription $ desc
  return name

-- Depending on whether PackageId refers to a "local" package, return
-- a relative path or the hackage url
computePath :: [PackageName] -> (PackageId -> FilePath)
computePath names =
  let pkgSet = Set.fromList names in \pkgId ->

  if pkgName pkgId `Set.member` pkgSet
    then
      printf "../%s"
        (display $ pkgName pkgId)
    else
      printf "http://hackage.haskell.org/packages/archive/%s/%s/doc/html"
        (display $ pkgName pkgId)
        (display $ pkgVersion pkgId)

main = do
  (pkgDbArgs, dest, pkgDirs) <- execParser $
    info (helper <*> optParser) idm

  -- make all paths absolute, since we'll be changing directories
  -- but first create dest — canonicalizePath will throw an exception if
  -- it's not there
  createDirectoryIfMissing True {- also parents -} dest
  dest <- canonicalizePath dest
  pkgDirs <- mapM canonicalizePath pkgDirs

  pkgNames <- getPackageNames pkgDirs

  let
    configFlags =
      (defaultConfigFlags defaultProgramConfiguration)
        { configPackageDBs = map (Just . SpecificPackageDB) pkgDbArgs }
    haddockFlags =
      defaultHaddockFlags 
        { haddockDistPref = Distribution.Simple.Setup.Flag dest }

  forM_ pkgDirs $ \dir -> do
    setCurrentDirectory dir

    lbi <- configureAction simpleUserHooks configFlags []
    haddockAction lbi simpleUserHooks haddockFlags [] (computePath pkgNames)
