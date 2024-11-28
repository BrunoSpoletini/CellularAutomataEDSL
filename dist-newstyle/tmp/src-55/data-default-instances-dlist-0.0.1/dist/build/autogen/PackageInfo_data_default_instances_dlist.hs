{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_data_default_instances_dlist (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "data_default_instances_dlist"
version :: Version
version = Version [0,0,1] []

synopsis :: String
synopsis = "Default instances for types in dlist"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
