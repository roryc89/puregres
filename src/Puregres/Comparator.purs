module Puregres.Comparator where

import Prelude (class Show)

data Comparator
  = Eq
  | Lt
  | Gt

instance showComparator :: Show Comparator where
  show Eq = " = "
  show Lt = " < "
  show Gt = " > "
