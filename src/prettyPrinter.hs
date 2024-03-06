module PrettyPrinter where

import Common
import Text.PrettyPrint.HughesPJ
import Prelude hiding ( (<>) )

printEnv :: [Comm] -> Doc
printOps [] = text ""
printOps [x] = text "  "
               <> text (show x)
               <> text "\n"
printOps (x:xs) = text "  "
                  <> text (show x)
                  <> text ","
                  <> text "\n"
                  <> printOps xs
