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

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.InstalledPackageInfo
import Control.Monad

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { postConf = \args flags pkg lbi ->
      do validateLicenses lbi
         postConf simpleUserHooks args flags pkg lbi
  }

validateLicenses :: LocalBuildInfo -> IO ()
validateLicenses lbi =
  do let p pkg = license pkg `notElem` freeLicenses
         badPkgs = filter p
                 $ allPackages
                 $ installedPkgs lbi

     unless (null badPkgs) $
       do mapM_ print badPkgs
          fail "BAD LICENSE"

freeLicenses :: [License]
freeLicenses = [ BSD2, BSD3, ISC, MIT ]
