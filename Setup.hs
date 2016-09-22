{-|
Module      : Main
Description : Custom setup script
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This is a default setup script except that it checks that all
transitive dependencies of this package use free licenses.

-}

module Main (main) where

import Control.Monad
import Data.Char
import Data.List
import Distribution.InstalledPackageInfo (InstalledPackageInfo, sourcePackageId, license)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription hiding (license)
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { postConf = \args flags pkg lbi ->
      do myPostConf args flags pkg lbi
         postConf simpleUserHooks args flags pkg lbi
  , sDistHook = \ pkg mbLbi hooks flags ->
      do let pkg' = forgetBuildModule pkg
         sDistHook simpleUserHooks pkg' mbLbi hooks flags
  }


forgetBuildModule :: PackageDescription -> PackageDescription
forgetBuildModule pkg =
  pkg { library     = forgetInLibrary    <$> library pkg
      , executables = forgetInExecutable <$> executables pkg
      , benchmarks  = forgetInBenchmark  <$> benchmarks pkg
      , testSuites  = forgetInTestSuite  <$> testSuites pkg
      }
  where
    forget = delete (ModuleName.fromString (buildModuleName pkg))

    forgetInBuildInfo bi = bi { otherModules = forget (otherModules bi) }

    forgetInLibrary lib = lib { exposedModules = forget (exposedModules lib)
                              , libBuildInfo = forgetInBuildInfo (libBuildInfo lib)
                              }

    forgetInTestSuite t = t { testBuildInfo = forgetInBuildInfo (testBuildInfo t)
                            }

    forgetInBenchmark b = b { benchmarkBuildInfo = forgetInBuildInfo (benchmarkBuildInfo b)
                            }

    forgetInExecutable e = e { buildInfo = forgetInBuildInfo (buildInfo e)
                             }


buildModuleName :: PackageDescription -> String
buildModuleName pkg = "Build_" ++ map clean (unPackageName (pkgName (package pkg)))
  where
    clean x | isAlphaNum x = x
            | otherwise    = '_'


-- | Custom hooks to check license acceptability and to record dependencies
-- in an generated module.
myPostConf ::
  [String]           {- ^ arguments               -} ->
  ConfigFlags        {- ^ configuration flags     -} ->
  PackageDescription {- ^ package description     -} ->
  LocalBuildInfo     {- ^ local build information -} ->
  IO ()
myPostConf _args flags pkg lbi =
  do let pkgs = allPackages (installedPkgs lbi)
     validateLicenses pkgs
     generateDepModules (fromFlag (configVerbosity flags)) pkg lbi pkgs


generateDepModules ::
  Verbosity              {- ^ build verbosity                 -} ->
  PackageDescription     {- ^ package description             -} ->
  LocalBuildInfo         {- ^ local build information         -} ->
  [InstalledPackageInfo] {- ^ transitive package dependencies -} ->
  IO ()
generateDepModules verbosity pkg lbi pkgs =
  do let dir = autogenModulesDir lbi
     createDirectoryIfMissingVerbose verbosity True dir
     rewriteFile (dir </> buildModuleName pkg ++ ".hs")
       $ unlines
       [ "module Build_" ++ unPackageName (pkgName (package pkg)) ++ " (deps) where"
       , ""
       , "deps :: [(String,[Int])]"
       , "deps = " ++ show [ (unPackageName (pkgName pid), versionBranch (pkgVersion pid))
                            | p <- pkgs
                            , let pid = sourcePackageId p ]
       ]


validateLicenses :: [InstalledPackageInfo] -> IO ()
validateLicenses pkgs =
  do let p pkg   = license pkg `notElem` freeLicenses
         badPkgs = filter p pkgs

     unless (null badPkgs) $
       do mapM_ print badPkgs
          fail "BAD LICENSE"


freeLicenses :: [License]
freeLicenses = [ BSD2, BSD3, ISC, MIT ]

