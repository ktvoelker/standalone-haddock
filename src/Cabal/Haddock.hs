-- Code copied or derived from Distribution.Simple.Haddock and Distribution.Client.Haddock.

-- We modify functions to accept a hook (PackageId -> FilePath) instead of
-- a template to compute the final html path of the package documentation

{-
Copyright (c) 2003-2008, Isaac Jones, Simon Marlow, Martin Sjögren,
                         Bjorn Bringert, Krasimir Angelov,
                         Malcolm Wallace, Ross Patterson, Ian Lynagh,
                         Duncan Coutts, Thomas Schilling
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module Cabal.Haddock
  ( haddock
  , hscolour
  , regenerateHaddockIndex
  ) where

-- local
import Distribution.Package
         ( PackageIdentifier(..)
         , Package(..)
         , PackageName(..), mkPackageName, packageName, mkLegacyUnitId, PackageId )
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), allExtensions
         , Library(..), hasLibs, Executable(..), specVersion )
import Distribution.Simple.Compiler
         ( Compiler(..), CompilerFlavor(GHC), compilerInfo, compilerVersion )
import Distribution.Simple.GHC ( componentGhcOptions, getLibDir )
import Distribution.Simple.Program.GHC ( GhcOptions(..), GhcDynLinkMode(..)
                                       , renderGhcOptions )
import Distribution.Simple.Program
import Distribution.Simple.PreProcess (ppCpp', ppUnlit
                                      , PPSuffixHandler, runSimplePreProcessor
                                      , preprocessComponent)
import Distribution.Simple.Setup
        ( defaultHscolourFlags, Flag(..), toFlag, flagToMaybe, flagToList, fromFlag
        , HaddockFlags(..), HscolourFlags(..) )
import Distribution.Simple.Build (initialBuildSteps)
import Distribution.Simple.InstallDirs
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), Component(..), ComponentLocalBuildInfo(..)
         , allLibModules, componentBuildDir, withAllComponentsInBuildOrder )
import Distribution.Simple.BuildPaths
         ( haddockName, autogenComponentModulesDir, autogenModulesDir, autogenPackageModulesDir )
import Distribution.Simple.PackageIndex (dependencyClosure)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, haddockInterfaces, sourcePackageId )
import Distribution.Simple.Utils
         ( die, copyFileTo, warn, notice, intercalate, setupMessage
         , createDirectoryIfMissingVerbose, withTempFileEx, copyFileVerbose
         , withTempDirectoryEx
         , findFileWithExtension, findFile
         , TempFileOptions(..), defaultTempFileOptions )
import Distribution.Simple.Glob ( matchDirFileGlob )
import Distribution.System (buildPlatform)
import Distribution.Text
         ( display, simpleParse )
import Distribution.Types.UnqualComponentName
import Distribution.Utils.NubList
         ( NubListR(), toNubListR, fromNubListR )

import Distribution.Verbosity
import Language.Haskell.Extension
-- Base
import System.Directory(removeFile, doesFileExist, createDirectoryIfMissing)

import Control.Monad ( when, guard, forM_ )
import Control.Exception (assert)
import Data.Monoid
import Data.Maybe    ( fromMaybe, listToMaybe )

import System.FilePath
import System.IO (hClose, hPutStrLn, hSetEncoding, utf8)
import Distribution.Version

-- Types

-- | record that represents the arguments to the haddock executable, a product monoid.
data HaddockArgs = HaddockArgs {
 argInterfaceFile :: Flag FilePath,               -- ^ path of the interface file, relative to argOutputDir, required.
 argPackageName :: Flag PackageIdentifier,        -- ^ package name,                                         required.
 argHideModules :: (All,[ModuleName.ModuleName]), -- ^ (hide modules ?, modules to hide)
 argIgnoreExports :: Any,                         -- ^ ignore export lists in modules?
 argLinkSource :: Any,                            -- ^ use hyperlinked-source?
 argCssFile :: Flag FilePath,                     -- ^ optional custom CSS file.
 argContents :: Flag String,                      -- ^ optional URL to contents page
 argVerbose :: Any,
 argOutput :: Flag [Output],                      -- ^ HTML or Hoogle doc or both?                                   required.
 argInterfaces :: [(FilePath, Maybe String)],     -- ^ [(interface file, URL to the HTML docs for links)]
 argOutputDir :: Directory,                       -- ^ where to generate the documentation.
 argTitle :: Flag String,                         -- ^ page's title,                                         required.
 argPrologue :: Flag String,                      -- ^ prologue text,                                        required.
 argGhcOptions :: Flag (GhcOptions, Version),     -- ^ additional flags to pass to ghc for haddock-2
 argGhcLibDir :: Flag FilePath,                   -- ^ to find the correct ghc,                              required by haddock-2.
 argTargets :: [FilePath]                         -- ^ modules to process.
}

-- | the FilePath of a directory, it's a monoid under (</>)
newtype Directory = Dir { unDir' :: FilePath } deriving (Read,Show,Eq,Ord)

unDir :: Directory -> FilePath
unDir = joinPath . filter (\p -> p /="./" && p /= ".") . splitPath . unDir'

data Output = Html | Hoogle

-- --------------------------------------------------------------------------
-- Haddock support

haddock :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HaddockFlags -> (PackageId -> FilePath) -> IO ()
haddock pkg_descr _ _ haddockFlags _computePath
  |    not (hasLibs pkg_descr)
    && not (fromFlag $ haddockExecutables haddockFlags) =
      warn (fromFlag $ haddockVerbosity haddockFlags) $
           "No documentation was generated as this package does not contain "
        ++ "a library. Perhaps you want to use the --executables flag."

haddock pkg_descr lbi suffixes flags computePath = do

    setupMessage verbosity "Running Haddock for" (packageId pkg_descr)
    (confHaddock, version, _) <-
      requireProgramVersion verbosity haddockProgram
        (orLaterVersion (mkVersion [0,6])) (withPrograms lbi)

    -- various sanity checks
    let isVersion2   = version >= mkVersion [2,0]

    when ( flag haddockHoogle
           && version > mkVersion [2]
           && version < mkVersion [2,2]) $
         die "haddock 2.0 and 2.1 do not support the --hoogle flag."

    when ( flag haddockLinkedSource
           && version < mkVersion [2, 16, 2]) $
         die "haddock does not support --hyperlinked-source before 2.16.2"

    when isVersion2 $ do
      haddockGhcVersionStr <- rawSystemProgramStdout verbosity confHaddock
                                ["--ghc-version"]
      case simpleParse haddockGhcVersionStr of
        Nothing -> die "Could not get GHC version from Haddock"
        Just haddockGhcVersion
          | haddockGhcVersion == ghcVersion -> return ()
          | otherwise -> die $
                 "Haddock's internal GHC version must match the configured "
              ++ "GHC version.\n"
              ++ "The GHC version is " ++ display ghcVersion ++ " but "
              ++ "haddock is using GHC version " ++ display haddockGhcVersion
          where ghcVersion = compilerVersion (compiler lbi)

    -- the tools match the requests, we can proceed

    initialBuildSteps (flag haddockDistPref) pkg_descr lbi verbosity

    when (flag haddockLinkedSource && version < mkVersion [2,17]) $
      hscolour' pkg_descr lbi suffixes $
         defaultHscolourFlags `mappend` haddockToHscolour flags

    libdirArgs <- getGhcLibDir  verbosity lbi isVersion2
    let pkg_id = packageId pkg_descr
    let commonArgs = mconcat
            [ libdirArgs
            -- Is it possible to get an InstalledPackageInfo here?
            -- If so, it would be better to get the package key from that.
            -- Using OldPackageKey might not work in all cases.
            , fromFlags (haddockTemplateEnv lbi pkg_id) flags
            , fromPackageDescription pkg_descr ]

    withAllComponentsInBuildOrder pkg_descr lbi $ \component clbi -> do
      preprocessComponent pkg_descr component lbi clbi False verbosity suffixes
      case component of
        CLib lib -> do
          withTempDirectoryEx verbosity tempFileOpts (buildDir lbi) "tmp" $ \tmp -> do
            let bi = libBuildInfo lib
            libArgs  <- fromLibrary verbosity tmp lbi lib clbi computePath
            libArgs' <- prepareSources verbosity tmp
                          lbi clbi isVersion2 bi (commonArgs `mappend` libArgs)
            runHaddock verbosity tempFileOpts comp confHaddock libArgs'
        CExe exe -> when (flag haddockExecutables) $ do
          withTempDirectoryEx verbosity tempFileOpts (buildDir lbi) "tmp" $ \tmp -> do
            let bi = buildInfo exe
            exeArgs  <- fromExecutable verbosity tmp lbi exe clbi computePath
            exeArgs' <- prepareSources verbosity tmp
                          lbi clbi isVersion2 bi (commonArgs `mappend` exeArgs)
            runHaddock verbosity tempFileOpts comp confHaddock exeArgs'
        _ -> return ()

    forM_ (extraDocFiles pkg_descr) $ \ fpath -> do
      files <- matchDirFileGlob verbosity (specVersion pkg_descr) "." fpath
      forM_ files $ copyFileTo verbosity (unDir $ argOutputDir commonArgs)
  where
    verbosity     = flag haddockVerbosity
    tempFileOpts  = defaultTempFileOptions
                    { optKeepTempFiles = flag haddockKeepTempFiles }
    flag f        = fromFlag $ f flags
    comp = compiler lbi
    -- htmlTemplate = fmap toPathTemplate . flagToMaybe . haddockHtmlLocation $ flags

-- | performs cpp and unlit preprocessing where needed on the files in
-- | argTargets, which must have an .hs or .lhs extension.
prepareSources :: Verbosity
                  -> FilePath
                  -> LocalBuildInfo
                  -> ComponentLocalBuildInfo
                  -> Bool            -- haddock >= 2.0
                  -> BuildInfo
                  -> HaddockArgs
                  -> IO HaddockArgs
prepareSources verbosity tmp lbi clbi isVersion2 bi args@HaddockArgs{argTargets=files} =
              mapM (mockPP tmp) files >>= \targets -> return args {argTargets=targets}
          where
            mockPP pref file = do
                 let (filePref, fileName) = splitFileName file
                     targetDir  = pref </> filePref
                     targetFile = targetDir </> fileName
                     (targetFileNoext, targetFileExt) = splitExtension $ targetFile
                     hsFile = targetFileNoext <.> "hs"

                 assert (targetFileExt `elem` [".lhs",".hs"]) $ return ()

                 createDirectoryIfMissing True targetDir

                 if needsCpp
                    then do
                      runSimplePreProcessor (ppCpp' defines bi lbi clbi)
                                            file targetFile verbosity
                    else
                      copyFileVerbose verbosity file targetFile

                 when (targetFileExt == ".lhs") $ do
                     runSimplePreProcessor ppUnlit targetFile hsFile verbosity
                     removeFile targetFile

                 return hsFile
            needsCpp = EnableExtension CPP `elem` allExtensions bi
            defines | isVersion2 = []
                    | otherwise  = ["-D__HADDOCK__"]

--------------------------------------------------------------------------------------------------
-- constributions to HaddockArgs

fromFlags :: PathTemplateEnv -> HaddockFlags -> HaddockArgs
fromFlags env flags =
    mempty {
      argHideModules = (maybe mempty (All . not) $ flagToMaybe (haddockInternal flags), mempty),
      argLinkSource = maybe mempty Any $ flagToMaybe $ haddockLinkedSource flags,
      argCssFile = haddockCss flags,
      argContents = fmap (fromPathTemplate . substPathTemplate env) (haddockContents flags),
      argVerbose = maybe mempty (Any . (>= deafening)) . flagToMaybe $ haddockVerbosity flags,
      argOutput =
          Flag $ case [ Html | Flag True <- [haddockHtml flags] ] ++
                      [ Hoogle | Flag True <- [haddockHoogle flags] ]
                 of [] -> [ Html ]
                    os -> os,
      argOutputDir = maybe mempty Dir . flagToMaybe $ haddockDistPref flags
    }

-- N.B.: argOutputDir generation algorithm is changed here
fromPackageDescription :: PackageDescription -> HaddockArgs
fromPackageDescription pkg_descr =
      mempty {
                argInterfaceFile = Flag $ haddockName pkg_descr,
                argPackageName = Flag $ packageId $ pkg_descr,
                -- argOutputDir = Dir $ "doc" </> "html" </> display (packageName pkg_descr),
                argOutputDir = Dir $ display (packageName pkg_descr),
                argPrologue = Flag $ if null desc then synopsis pkg_descr else desc,
                argTitle = Flag $ showPkg ++ subtitle
             }
      where
        desc = PD.description pkg_descr
        showPkg = display (packageId pkg_descr)
        subtitle | null (synopsis pkg_descr) = ""
                 | otherwise                 = ": " ++ synopsis pkg_descr

ghcSharedOptions :: BuildInfo
                 -> NubListR String
ghcSharedOptions = toNubListR . maybe [] id . lookup GHC . sharedOptions

fromLibrary :: Verbosity
            -> FilePath
            -> LocalBuildInfo -> Library -> ComponentLocalBuildInfo
            -> (PackageId -> FilePath)
            -> IO HaddockArgs
fromLibrary verbosity tmp lbi lib clbi htmlTemplate = do
    inFiles <- map snd `fmap` getLibSourceFiles verbosity lbi lib clbi
    ifaceArgs <- getInterfaces verbosity lbi clbi htmlTemplate
    let vanillaOpts = (componentGhcOptions normal lbi bi clbi (buildDir lbi)) {
                          -- Noooooooooo!!!!!111
                          -- haddock stomps on our precious .hi
                          -- and .o files. Workaround by telling
                          -- haddock to write them elsewhere.
                          ghcOptObjDir  = toFlag tmp,
                          ghcOptHiDir   = toFlag tmp,
                          ghcOptStubDir = toFlag tmp
                      }
        sharedOpts = vanillaOpts {
                         ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                         ghcOptFPic        = toFlag True,
                         ghcOptHiSuffix    = toFlag "dyn_hi",
                         ghcOptObjSuffix   = toFlag "dyn_o",
                         ghcOptExtra       = fromNubListR $ ghcSharedOptions bi
                     }
    opts <- if withVanillaLib lbi
            then return vanillaOpts
            else if withSharedLib lbi
            then return sharedOpts
            else die "Must have vanilla or shared libraries enabled in order to run haddock"
    return ifaceArgs {
      argHideModules = (mempty,otherModules $ bi),
      argGhcOptions  = toFlag (opts, ghcVersion),
      argTargets     = inFiles
    }
  where
    bi = libBuildInfo lib
    ghcVersion = compilerVersion (compiler lbi)

fromExecutable :: Verbosity
               -> FilePath
               -> LocalBuildInfo -> Executable -> ComponentLocalBuildInfo
               -> (PackageId -> FilePath)
               -> IO HaddockArgs
fromExecutable verbosity tmp lbi exe clbi computePath = do
    inFiles <- map snd `fmap` getExeSourceFiles verbosity lbi exe clbi
    ifaceArgs <- getInterfaces verbosity lbi clbi computePath
    let vanillaOpts = (componentGhcOptions normal lbi bi clbi (buildDir lbi)) {
                          -- Noooooooooo!!!!!111
                          -- haddock stomps on our precious .hi
                          -- and .o files. Workaround by telling
                          -- haddock to write them elsewhere.
                          ghcOptObjDir  = toFlag tmp,
                          ghcOptHiDir   = toFlag tmp,
                          ghcOptStubDir = toFlag tmp
                      }
        sharedOpts = vanillaOpts {
                         ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                         ghcOptFPic        = toFlag True,
                         ghcOptHiSuffix    = toFlag "dyn_hi",
                         ghcOptObjSuffix   = toFlag "dyn_o",
                         ghcOptExtra       = fromNubListR $ ghcSharedOptions bi
                     }
    opts <- if withVanillaLib lbi
            then return vanillaOpts
            else if withSharedLib lbi
            then return sharedOpts
            else die "Must have vanilla or shared libraries enabled in order to run haddock"
    return ifaceArgs {
      argGhcOptions = toFlag (opts, ghcVersion),
      argOutputDir  = Dir (unUnqualComponentName $ exeName exe),
      argTitle      = Flag (unUnqualComponentName $ exeName exe),
      argTargets    = inFiles
    }
  where
    bi = buildInfo exe
    ghcVersion = compilerVersion (compiler lbi)

getInterfaces :: Verbosity
              -> LocalBuildInfo
              -> ComponentLocalBuildInfo
              -> (PackageId -> FilePath)
              -> IO HaddockArgs
getInterfaces verbosity lbi clbi computePath = do
    (packageFlags, warnings) <- haddockPackageFlags lbi clbi computePath
    maybe (return ()) (warn verbosity) warnings
    return $ mempty {
                 argInterfaces = packageFlags
               }

getGhcLibDir :: Verbosity -> LocalBuildInfo
             -> Bool -- ^ are we using haddock-2.x ?
             -> IO HaddockArgs
getGhcLibDir verbosity lbi isVersion2
    | isVersion2 =
        do l <- getLibDir verbosity lbi
           return $ mempty { argGhcLibDir = Flag l }
    | otherwise  =
        return mempty

----------------------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------
-- | Call haddock with the specified arguments.
runHaddock :: Verbosity
              -> TempFileOptions
              -> Compiler
              -> ConfiguredProgram
              -> HaddockArgs
              -> IO ()
runHaddock verbosity tmpFileOpts comp confHaddock args = do
  let haddockVersion = fromMaybe (error "unable to determine haddock version")
                       (programVersion confHaddock)
  renderArgs verbosity tmpFileOpts haddockVersion comp args $
    \(flags,result)-> do

      rawSystemProgram verbosity confHaddock flags

      notice verbosity $ "Documentation created: " ++ result


renderArgs :: Verbosity
              -> TempFileOptions
              -> Version
              -> Compiler
              -> HaddockArgs
              -> (([String], FilePath) -> IO a)
              -> IO a
renderArgs verbosity tmpFileOpts version comp args k = do
  createDirectoryIfMissingVerbose verbosity True outputDir
  withTempFileEx tmpFileOpts outputDir "haddock-prolog.txt" $ \prologFileName h -> do
          do
             when (version >= mkVersion [2,15]) (hSetEncoding h utf8)
             hPutStrLn h $ fromFlag $ argPrologue args
             hClose h
             let pflag = "--prologue=" ++ prologFileName
             k (pflag : renderPureArgs version comp args, result)
    where
      isVersion2 = version >= mkVersion [2,0]
      outputDir = (unDir $ argOutputDir args)
      result = intercalate ", "
             . map (\o -> outputDir </>
                            case o of
                              Html -> "index.html"
                              Hoogle -> pkgstr <.> "txt")
             $ arg argOutput
            where
              pkgstr | isVersion2 = display $ packageName pkgid
                     | otherwise = display pkgid
              pkgid = arg argPackageName
      arg f = fromFlag $ f args

renderPureArgs :: Version -> Compiler -> HaddockArgs -> [String]
renderPureArgs version comp args = concat
    [
     (:[]) . (\f -> "--dump-interface="++ unDir (argOutputDir args) </> f)
     . fromFlag . argInterfaceFile $ args,
     (\pname ->   if isVersion2
                  then ["--optghc=-package-name", "--optghc=" ++ pname]
                  else ["--package=" ++ pname]) . display . fromFlag . argPackageName $ args,
     (\(All b,xs) -> bool (map (("--hide=" ++). display) xs) [] b) . argHideModules $ args,
     bool ["--ignore-all-exports"] [] . getAny . argIgnoreExports $ args,
     bool ["--hyperlinked-source"] [] . getAny . argLinkSource $ args,
     maybe [] ((:[]).("--css="++)) . flagToMaybe . argCssFile $ args,
     maybe [] ((:[]).("--use-contents="++)) . flagToMaybe . argContents $ args,
     bool [] [verbosityFlag] . getAny . argVerbose $ args,
     map (\o -> case o of Hoogle -> "--hoogle"; Html -> "--html") . fromFlag . argOutput $ args,
     renderInterfaces . argInterfaces $ args,
     (:[]).("--odir="++) . unDir . argOutputDir $ args,
     (:[]).("--title="++) . (bool (++" (internal documentation)") id (getAny $ argIgnoreExports args))
              . fromFlag . argTitle $ args,
     [ "--optghc=" ++ opt | isVersion2
                          , (opts, _ghcVer) <- flagToList (argGhcOptions args)
                          , opt <- renderGhcOptions comp buildPlatform opts ],
     maybe [] (\l -> ["-B"++l]) $ guard isVersion2 >> flagToMaybe (argGhcLibDir args), -- error if isVersion2 and Nothing?
     argTargets $ args
    ]
    where
      renderInterfaces =
        map (\(i,mh) -> "--read-interface=" ++
          maybe "" (++",") mh ++ i)
      bool a b c = if c then a else b
      isVersion2    = version >= mkVersion [2,0]
      isVersion2_5  = version >= mkVersion [2,5]
      verbosityFlag
       | isVersion2_5 = "--verbosity=1"
       | otherwise = "--verbose"


-----------------------------------------------------------------------------------------------------------

haddockPackageFlags :: LocalBuildInfo
                    -> ComponentLocalBuildInfo
                    -> (PackageId -> FilePath)
                    -> IO ([(FilePath,Maybe FilePath)], Maybe String)
haddockPackageFlags lbi clbi computePath = do
  let allPkgs = installedPkgs lbi
      directDeps = map fst (componentPackageDeps clbi)
  transitiveDeps <- case dependencyClosure allPkgs directDeps of
    Left x    -> return x
    Right inf -> die $ "internal error when calculating transative "
                    ++ "package dependencies.\nDebug info: " ++ show inf
  interfaces <- sequence
    [ case interfaceAndHtmlPath ipkg of
        Nothing -> return (Left (packageId ipkg))
        Just (interface, html) -> do
          exists <- doesFileExist interface
          if exists
            then return (Right (interface, html))
            else return (Left (packageId ipkg))
    | ipkg <- PackageIndex.allPackages transitiveDeps
    , pkgName (packageId ipkg) `notElem` noHaddockWhitelist
    ]

  let missing = [ pkgid | Left pkgid <- interfaces ]
      warning = "The documentation for the following packages are not "
             ++ "installed. No links will be generated to these packages: "
             ++ intercalate ", " (map display missing)
      flags = [ (interface, if null html then Nothing else Just html)
              | Right (interface, html) <- interfaces ]

  return (flags, if null missing then Nothing else Just warning)

  where
    noHaddockWhitelist = map mkPackageName [ "rts" ]
    interfaceAndHtmlPath :: InstalledPackageInfo -> Maybe (FilePath, FilePath)
    interfaceAndHtmlPath pkg = do
      interface <- listToMaybe (haddockInterfaces pkg)
      let html = computePath (sourcePackageId pkg)
      return (interface, html)

haddockTemplateEnv :: LocalBuildInfo -> PackageIdentifier -> PathTemplateEnv
haddockTemplateEnv lbi pkg_id = (PrefixVar, prefix (installDirTemplates lbi))
                         : initialPathTemplateEnv pkg_id (mkLegacyUnitId pkg_id) (compilerInfo (compiler lbi))
                           (hostPlatform lbi)

-- --------------------------------------------------------------------------
-- hscolour support

hscolour :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HscolourFlags -> IO ()
hscolour pkg_descr lbi suffixes flags = do
  -- we preprocess even if hscolour won't be found on the machine
  -- will this upset someone?
  initialBuildSteps distPref pkg_descr lbi verbosity
  hscolour' pkg_descr lbi suffixes flags
 where
   verbosity  = fromFlag (hscolourVerbosity flags)
   distPref = fromFlag $ hscolourDistPref flags

hscolour' :: PackageDescription
          -> LocalBuildInfo
          -> [PPSuffixHandler]
          -> HscolourFlags
          -> IO ()
hscolour' pkg_descr lbi suffixes flags = do
    let distPref = fromFlag $ hscolourDistPref flags

    -- Upstream hscolourPref is a function in Distribution.Simple.BuildPaths
    -- defined as:
    --
    --    distPref </> "doc" </> "html" </> display (packageName pkg_descr)
    --
    -- However, standalone-haddock does not put the docs in
    -- <dest>/doc/html/<package-name>, but <dest>/<package-name>.
    let hscolourPref = distPref </> display (packageName pkg_descr)

    (hscolourProg, _, _) <-
      requireProgramVersion
        verbosity hscolourProgram
        (orLaterVersion (mkVersion [1,8])) (withPrograms lbi)

    setupMessage verbosity "Running hscolour for" (packageId pkg_descr)
    createDirectoryIfMissingVerbose verbosity True hscolourPref

    withAllComponentsInBuildOrder pkg_descr lbi $ \comp clbi -> do
      preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
      case comp of
        CLib lib -> do
          let outputDir = hscolourPref </> "src"
          runHsColour hscolourProg outputDir =<< getLibSourceFiles verbosity lbi lib clbi
        CExe exe | fromFlag (hscolourExecutables flags) -> do
          let outputDir = hscolourPref </> unUnqualComponentName (exeName exe) </> "src"
          runHsColour hscolourProg outputDir =<< getExeSourceFiles verbosity lbi exe clbi
        _ -> return ()
  where
    stylesheet = flagToMaybe (hscolourCSS flags)

    verbosity  = fromFlag (hscolourVerbosity flags)

    runHsColour prog outputDir moduleFiles = do
         createDirectoryIfMissingVerbose verbosity True outputDir

         case stylesheet of -- copy the CSS file
           Nothing | programVersion prog >= Just (mkVersion [1,9]) ->
                       rawSystemProgram verbosity prog
                          ["-print-css", "-o" ++ outputDir </> "hscolour.css"]
                   | otherwise -> return ()
           Just s -> copyFileVerbose verbosity s (outputDir </> "hscolour.css")

         forM_ moduleFiles $ \(m, inFile) ->
             rawSystemProgram verbosity prog
                    ["-css", "-anchor", "-o" ++ outFile m, inFile]
        where
          outFile m = outputDir </> intercalate "-" (ModuleName.components m) <.> "html"

haddockToHscolour :: HaddockFlags -> HscolourFlags
haddockToHscolour flags =
    HscolourFlags {
      hscolourCSS         = haddockHscolourCss flags,
      hscolourExecutables = haddockExecutables flags,
      hscolourTestSuites  = haddockTestSuites  flags,
      hscolourBenchmarks  = haddockBenchmarks  flags,
      hscolourVerbosity   = haddockVerbosity   flags,
      hscolourDistPref    = haddockDistPref    flags
    }

----------------------------------------------------------------------------------------------
-- TODO these should be moved elsewhere.

getLibSourceFiles :: Verbosity
                     -> LocalBuildInfo
                     -> Library
                     -> ComponentLocalBuildInfo
                     -> IO [(ModuleName.ModuleName, FilePath)]
getLibSourceFiles verbosity lbi lib clbi = getSourceFiles verbosity searchpaths modules
  where
    bi               = libBuildInfo lib
    modules          = allLibModules lib clbi
    searchpaths      = componentBuildDir lbi clbi : hsSourceDirs bi ++
                     [ autogenComponentModulesDir lbi clbi
                     , autogenPackageModulesDir lbi ]

getExeSourceFiles :: Verbosity
                     -> LocalBuildInfo
                     -> Executable
                     -> ComponentLocalBuildInfo
                     -> IO [(ModuleName.ModuleName, FilePath)]
getExeSourceFiles verbosity lbi exe clbi = do
    moduleFiles <- getSourceFiles verbosity searchpaths modules
    srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)
    return ((ModuleName.main, srcMainPath) : moduleFiles)
  where
    bi          = buildInfo exe
    modules     = otherModules bi
    searchpaths = autogenComponentModulesDir lbi clbi
                : autogenPackageModulesDir lbi
                : exeBuildDir lbi exe : hsSourceDirs bi

getSourceFiles :: Verbosity -> [FilePath]
                  -> [ModuleName.ModuleName]
                  -> IO [(ModuleName.ModuleName, FilePath)]
getSourceFiles verbosity dirs modules = flip traverse modules $ \m -> fmap ((,) m) $
    findFileWithExtension ["hs", "lhs", "hsig", "lhsig"] dirs (ModuleName.toFilePath m)
      >>= maybe (notFound m) (return . normalise)
  where
    notFound module_ = die $ "can't find source for module " ++ display module_

-- | The directory where we put build results for an executable
exeBuildDir :: LocalBuildInfo -> Executable -> FilePath
exeBuildDir lbi exe = buildDir lbi </> nm </> nm ++ "-tmp"
  where nm = unUnqualComponentName $ exeName exe

---------------------------------------------------------------------------------------------




-- boilerplate monoid instance.
instance Monoid HaddockArgs where
    mempty = HaddockArgs {
                argInterfaceFile = mempty,
                argPackageName = mempty,
                argHideModules = mempty,
                argIgnoreExports = mempty,
                argLinkSource = mempty,
                argCssFile = mempty,
                argContents = mempty,
                argVerbose = mempty,
                argOutput = mempty,
                argInterfaces = mempty,
                argOutputDir = mempty,
                argTitle = mempty,
                argPrologue = mempty,
                argGhcOptions = mempty,
                argGhcLibDir = mempty,
                argTargets = mempty
             }
instance Semigroup HaddockArgs where
    a <> b = HaddockArgs {
                argInterfaceFile = mult argInterfaceFile,
                argPackageName = mult argPackageName,
                argHideModules = mult argHideModules,
                argIgnoreExports = mult argIgnoreExports,
                argLinkSource = mult argLinkSource,
                argCssFile = mult argCssFile,
                argContents = mult argContents,
                argVerbose = mult argVerbose,
                argOutput = mult argOutput,
                argInterfaces = mult argInterfaces,
                argOutputDir = mult argOutputDir,
                argTitle = mult argTitle,
                argPrologue = mult argPrologue,
                argGhcOptions = mult argGhcOptions,
                argGhcLibDir = mult argGhcLibDir,
                argTargets = mult argTargets
             }
      where mult f = f a `mappend` f b

instance Monoid Directory where
    mempty = Dir "."
instance Semigroup Directory where
    Dir m <> Dir n = Dir $ m </> n


regenerateHaddockIndex
  :: Verbosity
  -> ProgramConfiguration
  -> FilePath -- ^ dest dir
  -> [(FilePath, FilePath)] -- ^ [(interface, html)]
  -> IO ()
regenerateHaddockIndex verbosity conf dest paths = do
      (confHaddock, _, _) <-
          requireProgramVersion verbosity haddockProgram
                                    (orLaterVersion (mkVersion [0,6])) conf

      let flags = [ "--gen-contents"
                  , "--gen-index"
                  , "--odir=" ++ dest
                  , "--title=Standalone Haskell documentation" ]
               ++ [ "--read-interface=" ++ mkUrl html ++ "," ++ interface
                  | (interface, html) <- paths ]
      rawSystemProgram verbosity confHaddock flags


-- See https://github.com/haskell/cabal/issues/1064
mkUrl :: String -> String
mkUrl f =
  if isAbsolute f
    then "file://" ++ f
    else f
