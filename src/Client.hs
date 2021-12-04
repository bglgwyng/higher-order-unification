{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Client (infer) where

import Common
import Control.Applicative
import Control.Monad
import Control.Monad.Gen
import Control.Monad.Trans
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid hiding (Ap)
import qualified Data.Set as S
import Debug.Trace
import Unification

traceMonad :: (Show a, Monad m) => a -> m a
traceMonad x = trace ("test: " ++ show x) (return x)

typeOf ::
  M.Map String Inhabitant ->
  M.Map Id Term ->
  M.Map Id Term ->
  Term ->
  UnifyM (Term, S.Set Constraint)
typeOf gctx mcxt cxt t =
  case t of
    LocalVar i -> mzero
    FreeVar i -> foldMap (\x -> return (x, S.empty)) $ M.lookup i cxt
    MetaVar i -> mzero
    GlobalVar i ->
      maybe
        undefined
        ( \case
            Declaration x -> pure (x, mempty)
            Definition x _ -> pure (x, mempty)
        )
        $ M.lookup i gctx
    Uni -> return (Uni, S.empty)
    Ap l r -> do
      (tpl, cl) <- typeOf' mcxt cxt l
      case tpl of
        Pi from to -> do
          optional (typeOf' mcxt cxt r)
            <&> (subst r 0 to,) . (cl <>) . foldMap (\(tpr, cr) -> cr <> S.singleton (from, tpr))
        t -> error $ show t
    Lam arg b -> do
      v <- lift gen
      (to, cs) <-
        typeOf
          gctx
          mcxt
          (M.insert v arg cxt)
          (subst (FreeVar v) 0 b)
      return
        ( Pi arg (substFV (LocalVar 0) v (raise 1 to)),
          cs <> S.singleton (arg, arg)
        )
    Pi from to -> do
      v <- lift gen
      maybeFromUnification <- optional $ typeOf' mcxt cxt from
      (toTp, toCs) <- typeOf' mcxt (M.insert v from cxt) (subst (FreeVar v) 0 to)
      return
        ( Uni,
          foldMap snd maybeFromUnification
            <> toCs
            <> foldMap (S.singleton . (Uni,) . fst) maybeFromUnification
            <> S.singleton (Uni, toTp)
        )
  where
    typeOf' = typeOf gctx

infer :: M.Map String Inhabitant -> Term -> Maybe (Term, Subst, S.Set Constraint)
infer gctx t = listToMaybe . runUnifyM $ go
  where
    go = do
      (tp, cs) <- typeOf gctx M.empty M.empty t
      (subst, flexflex) <- unify Context {metas = M.empty} cs
      return (manySubst subst tp, subst, cs)
