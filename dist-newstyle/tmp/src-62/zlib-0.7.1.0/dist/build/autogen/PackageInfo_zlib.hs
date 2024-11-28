{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_zlib (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "zlib"
version :: Version
version = Version [0,7,1,0] []

synopsis :: String
synopsis = "Compression and decompression in the gzip and zlib formats"
copyright :: String
copyright = "(c) 2006-2016 Duncan Coutts"
homepage :: String
homepage = ""
