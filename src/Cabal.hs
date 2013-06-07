-- Code copied or derived from Cabal, because it is not part of Cabal's
-- public API
-- (mostly from Distribution.Simple)
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

module Cabal
  ( PackageDB(..)
  , ConfigFlags(..)
  , configureAction
  , haddockAction
  , simpleUserHooks
  , defaultConfigFlags
  , defaultHaddockFlags
  , defaultProgramConfiguration
  , PackageName(..)
  , PackageIdentifier(..)
  , PackageDescription(..)
  , GenericPackageDescription(..)
  , findPackageDesc
  , readPackageDescription
  , silent
  )
  where

-- We modify this function so that it accepts a hook to determine the html
-- location of each package
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
    | ipkg <- allPackages transitiveDeps
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
    noHaddockWhitelist = map PackageName [ "rts" ]
    interfaceAndHtmlPath :: InstalledPackageInfo -> Maybe (FilePath, FilePath)
    interfaceAndHtmlPath pkg = do
      interface <- listToMaybe (haddockInterfaces pkg)
      let html = computePath (sourcePackageId pkg)
      return (interface, html)

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

