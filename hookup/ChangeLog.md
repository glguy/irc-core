# Revision history for hookup

## 0.4

* Added ability to specify TLS private key password
* Replace protocol family selection with more general bind hostname selection
* Implement staggered, concurrent connection strategy based on RFC 8305

## 0.3.1.0 -- 2020-02

* Added `getClientCertificate`

## 0.3.0.1 -- 2020-01

* Remove extra-libraries section from cabal file to allow package to work on GHC 8.8.2

## 0.3 -- 2019-07

* Changed the hostname resolution exception constructor to be more useful

## 0.2.3 -- 2019-05

* Added functions to get TLS peer certificate information

## 0.2.1 -- 2018-07

* Added `connectWithSocket`, `recv`, `putBuf`, `defaultTlsParams`

## 0.2 -- 2017-11-22

* Allow connection parameters to specify address family with `cpFamily` field

## 0.1.1.0  -- 2017-05-13

* Better error message for old openssl version
* Nicer displayedExceptions
* Dropped unused template-haskell dependency
* More haddock comments

## 0.1.0.0  -- 2016-10-05

* First version. Released on an unsuspecting world.
