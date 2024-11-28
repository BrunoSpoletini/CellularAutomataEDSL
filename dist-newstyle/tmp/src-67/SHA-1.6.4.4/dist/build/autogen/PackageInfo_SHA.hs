{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_SHA (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "SHA"
version :: Version
version = Version [1,6,4,4] []

synopsis :: String
synopsis = "Implementations of the SHA suite of message digest functions"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
