module Main where

import Client (infer)
import Common
import Data.Map (fromList)
import Unification (Term (..), driver)

nat = gv "nat"

plus = gv "plus"

bool = gv "bool"

list = gv "list"

pure = gv "pure"

l0 = gv "l0"

inhabit = (gv "inhabit" @:)

map' = gv "map"

id' = gv "id"

globals :: GlobalContext
globals =
  fromList
    [ ("nat", Declaration Uni),
      ("list", Declaration $ Uni --> Uni),
      ("bool", Declaration Uni),
      ("z", Declaration nat),
      ("true", Declaration bool),
      ("f", Declaration (Uni --> (lv 0 --> lv 1) --> lv 1)),
      ("id", Declaration (Uni --> lv 0 --> lv 1)),
      ("pure", Declaration (Uni --> lv 0 --> list @: lv 1)),
      ("map", Declaration (Uni --> Uni --> (lv 1 --> lv 1) --> Ap list (lv 2) --> Ap list (lv 2))),
      ("l0", Declaration $ list @: nat),
      ("inhabit", Declaration $ Uni --> lv 0)
    ]

main = do
  mapM_ print $
    infer globals
      <$> [ Lam bool $ lv 0,
            map' @: mv 0 @: (list @: nat) @: (inhabit (Uni --> lv 0 --> list @: lv 1) @: mv 1) @: inhabit (list @: nat),
            inhabit (mv 0 --> list @: lv 0) @: inhabit nat,
            map' @: mv 0 @: (list @: nat) @: (inhabit (Uni --> mv 1 --> list @: lv 1) @: mv 2) @: inhabit (list @: nat),
            map' @: mv 0 @: (list @: nat) @: inhabit (mv 1 --> list @: nat),
            map' @: mv 0 @: mv 1 @: (gv "pure" @: mv 2) @: inhabit (list @: nat),
            mv 0 --> nat,
            mv 0 --> mv 1 --> nat
          ]