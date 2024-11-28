{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_foldable1_classes_compat (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "foldable1_classes_compat"
version :: Version
version = Version [0,1] []

synopsis :: String
synopsis = "Compatibility package for the Foldable1 and Bifoldable1 type classes"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/haskell-compat/foldable1-classes-compat"
