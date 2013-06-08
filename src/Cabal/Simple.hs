-- Code copied or derived from Distribution.Simple, because it's not part
-- of Cabal's public API
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
module Cabal.Simple
  ( configureAction
  , haddockAction
  , simpleUserHooks
  ) where

import Distribution.Simple.UserHooks
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple.Program
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils

-- our version of haddock launcher
import Cabal.Haddock

configureAction :: UserHooks -> ConfigFlags -> Args -> IO LocalBuildInfo
configureAction hooks flags args = do
                let distPref = fromFlag $ configDistPref flags
                pbi <- preConf hooks args flags

                (mb_pd_file, pkg_descr0) <- confPkgDescr

                --    get_pkg_descr (configVerbosity flags')
                --let pkg_descr = updatePackageDescription pbi pkg_descr0
                let epkg_descr = (pkg_descr0, pbi)

                --(warns, ers) <- sanityCheckPackage pkg_descr
                --errorOut (configVerbosity flags') warns ers

                localbuildinfo0 <- confHook hooks epkg_descr flags

                -- remember the .cabal filename if we know it
                -- and all the extra command line args
                let localbuildinfo = localbuildinfo0 {
                                       pkgDescrFile = mb_pd_file,
                                       extraConfigArgs = args
                                     }
                writePersistBuildConfig distPref localbuildinfo

                let pkg_descr = localPkgDescr localbuildinfo
                postConf hooks args flags pkg_descr localbuildinfo
                return localbuildinfo
              where
                verbosity = fromFlag (configVerbosity flags)
                confPkgDescr :: IO (Maybe FilePath, GenericPackageDescription)
                confPkgDescr = do
                  mdescr <- readDesc hooks
                  case mdescr of
                    Just descr -> return (Nothing, descr)
                    Nothing -> do
                      pdfile <- defaultPackageDesc verbosity
                      descr  <- readPackageDescription verbosity pdfile
                      return (Just pdfile, descr)
{-
hookedAction :: (UserHooks -> Args -> flags -> IO HookedBuildInfo)
        -> (UserHooks -> PackageDescription -> LocalBuildInfo
                      -> UserHooks -> flags -> IO ())
        -> (UserHooks -> Args -> flags -> PackageDescription
                      -> LocalBuildInfo -> IO ())
        -> IO LocalBuildInfo
        -> UserHooks -> flags -> Args -> IO ()
hookedAction pre_hook cmd_hook =
    hookedActionWithArgs pre_hook (\h _ pd lbi uh flags -> cmd_hook h pd lbi uh flags)

hookedActionWithArgs :: (UserHooks -> Args -> flags -> IO HookedBuildInfo)
        -> (UserHooks -> Args -> PackageDescription -> LocalBuildInfo
                      -> UserHooks -> flags -> IO ())
        -> (UserHooks -> Args -> flags -> PackageDescription
                      -> LocalBuildInfo -> IO ())
        -> IO LocalBuildInfo
        -> UserHooks -> flags -> Args -> IO ()
hookedActionWithArgs pre_hook cmd_hook post_hook get_build_config hooks flags args = do
   pbi <- pre_hook hooks args flags
   localbuildinfo <- get_build_config
   let pkg_descr0 = localPkgDescr localbuildinfo
   --pkg_descr0 <- get_pkg_descr (get_verbose flags)
   sanityCheckHookedBuildInfo pkg_descr0 pbi
   let pkg_descr = updatePackageDescription pbi pkg_descr0
   -- TODO: should we write the modified package descr back to the
   -- localbuildinfo?
   cmd_hook hooks args pkg_descr localbuildinfo hooks flags
   post_hook hooks args flags pkg_descr localbuildinfo

sanityCheckHookedBuildInfo :: PackageDescription -> HookedBuildInfo -> IO ()
sanityCheckHookedBuildInfo PackageDescription { library = Nothing } (Just _,_)
    = die $ "The buildinfo contains info for a library, "
         ++ "but the package does not have a library."

sanityCheckHookedBuildInfo pkg_descr (_, hookExes)
    | not (null nonExistant)
    = die $ "The buildinfo contains info for an executable called '"
         ++ head nonExistant ++ "' but the package does not have a "
         ++ "executable with that name."
  where
    pkgExeNames  = nub (map exeName (executables pkg_descr))
    hookExeNames = nub (map fst hookExes)
    nonExistant  = hookExeNames \\ pkgExeNames

sanityCheckHookedBuildInfo _ _ = return ()
-}
haddockAction :: LocalBuildInfo -> UserHooks -> HaddockFlags -> Args -> (PackageId -> FilePath) -> IO ()
haddockAction lbi _hooks flags _args computePath = do
  -- let distPref  = fromFlag $ haddockDistPref flags
  let verbosity = fromFlag $ haddockVerbosity flags

  _progs <- reconfigurePrograms verbosity
             (haddockProgramPaths flags)
             (haddockProgramArgs flags)
             (withPrograms lbi)

  let pkg_descr = localPkgDescr lbi
  haddock pkg_descr lbi knownSuffixHandlers flags computePath

simpleUserHooks :: UserHooks
simpleUserHooks = emptyUserHooks { confHook  = configure }
