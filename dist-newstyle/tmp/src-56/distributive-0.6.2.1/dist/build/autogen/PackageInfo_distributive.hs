{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_distributive (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "distributive"
version :: Version
version = Version [0,6,2,1] []

synopsis :: String
synopsis = "Distributive functors -- Dual to Traversable"
copyright :: String
copyright = "Copyright (C) 2011-2016 Edward A. Kmett"
homepage :: String
homepage = "http://github.com/ekmett/distributive/"
