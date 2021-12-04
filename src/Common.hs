module Common where

import Data.List
import qualified Data.Map as M

type Id = Int

type Index = Int

data Term
  = FreeVar Id
  | GlobalVar String
  | LocalVar Index
  | MetaVar Id
  | Uni
  | Ap Term Term
  | Lam Term Term
  | Pi Term Term
  deriving (Eq, Ord)

instance Show Term where
  showsPrec _ (FreeVar i) = shows i . showString "$"
  showsPrec _ (GlobalVar n) = showString n
  showsPrec _ (LocalVar i) = shows i . showString "!"
  showsPrec _ (MetaVar i) = shows i . showString "?"
  showsPrec _ Uni = showString "Type"
  showsPrec prec (Ap t1 t2) = showParen (prec > 3) $ showsPrec 3 t1 . showString " " . showsPrec 4 t2
  showsPrec prec (Lam t1 t2) = showParen (prec > 0) $ showString "\\" . shows t2
  showsPrec prec (Pi t1 t2) = showParen (prec > 1) $ showsPrec 2 t1 . showString " -> " . showsPrec 1 t2

infixr 8 -->

(-->) :: Term -> Term -> Term
(-->) = Pi

infixl 9 @:

(@:) :: Term -> Term -> Term
(@:) = Ap

fv = FreeVar

gv = GlobalVar

lv = LocalVar

mv = MetaVar

data Inhabitant = Declaration Term | Definition Term Term

type GlobalContext = M.Map String Inhabitant