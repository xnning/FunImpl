{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module TypeCheck (oneStep, typecheck, eval) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import qualified Data.Text as T
import           Lens.Micro
import           Unbound.Generics.LocallyNameless

import           PrettyPrint
import           Syntax
import           Environment

done :: MonadPlus m => m a
done = mzero

-- | Small step, call-by-value operational semantics
step :: Expr -> MaybeT FreshM Expr
step (Var{}) = done
step (Kind{}) = done
step (Lam{}) = done
step (Pi{}) = done
step (Lit{}) = done
step (Nat) = done
step (App (Lam bnd) t2) = do
  (delta, b) <- unbind bnd
  let (b', e') = multiSubst delta t2 b
  case b' of
    Empty -> return e'
    _     -> return (Lam (bind b' e'))
step (App t1 t2) =
  App <$> step t1 <*> pure t2
  <|> App <$> pure t1 <*> step t2
step (Let bnd) = do
  ((n, Embed e), b) <- unbind bnd
  let n' = name2String n
  elet n' <$> step e <*> pure b <|> pure (subst n e b)
step (PrimOp op (Lit n) (Lit m)) = do
  let x = evalOp op
  return (Lit (n `x` m))
step (PrimOp op e1 e2) =
  PrimOp <$> pure op <*> step e1 <*> pure e2
  <|> PrimOp <$> pure op <*> pure e1 <*> step e2

evalOp :: Operation -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mult = (*)

-- | transitive closure of `step`
tc :: (Monad m, Functor m) => (a -> MaybeT m a) -> a -> m a
tc f a = do
  ma' <- runMaybeT (f a)
  case ma' of
    Nothing -> return a
    Just a' -> tc f a'

eval :: Expr -> Expr
eval x = runFreshM (tc step x)

-- | type checker with positivity and contractiveness test
typecheck :: ClassTag -> Expr -> (Either T.Text Expr)
typecheck tag = runTcMonad (initialEnv tag) . infer

infer :: Expr -> TcMonad Expr
infer (Var x) = do
  (tag', pos') <- lookupCtx
  (t, theta) <- lookupTy x
  when ((tag', theta, pos') == (Logic, Prog, Neg)) $
    throwError $ T.concat
                   [ "Recursive variable "
                   , T.pack . show $ x
                   , " should not appear in a negative position"
                   ]
  return t
infer (Kind Star) = return (Kind Box)
infer (Lam bnd) = do
  (delta, m) <- unbind bnd
  b <- extendCtx delta (infer m)
  let retype = Pi $ bind delta b
  -- checkSort retype -- TODO: why?
  return retype
infer (App m n) = do
  bnd <- unPi =<< infer m
  (delta, b) <- unbind bnd
  checkArg n delta
  let (delta', b') = multiSubst delta n b
  case delta' of
    Empty -> return b'
    _     -> return (Pi (bind delta' b'))
infer e@(Pi bnd) = do
  (delta, b) <- unbind bnd
  checkDelta delta
  t <- extendCtx delta (infer b)
  unless (aeq t estar || aeq t ebox) $
    throwError $ T.concat [showExpr b, " should have sort ⋆ or □"]
  return t
infer (Let bnd) = do
  ((x, Embed e), b) <- unbind bnd
  t <- infer e
  t' <- infer (subst x e b)
  return (subst x e t') -- FIXME: overkill?
infer Nat = return estar
infer (Lit{}) = return Nat
infer (PrimOp{}) = return Nat
infer e = throwError $ T.concat ["Type checking ", showExpr e, " falied"]


-- helper functions

check :: Expr -> Expr -> TcMonad ()
check m a = do
  b <- infer m
  checkEq b a

checkArg :: Expr -> Tele -> TcMonad ()
checkArg _ Empty = return ()
checkArg e (Cons rb) = do
  let ((x, _, Embed a), t') = unrebind rb
  check e a

checkDelta :: Tele -> TcMonad ()
checkDelta Empty = return ()
checkDelta (Cons bnd) = do
  flipPos (checkSort t)
  extendCtx (mkTele [(name2String x, t)]) (checkDelta t')

  where
    ((x, _, Embed t), t') = unrebind bnd

checkSort :: Expr -> TcMonad ()
checkSort e = do
  t <- infer e
  unless (aeq t estar || aeq t ebox) $
    throwError $ T.concat [showExpr e, " should have sort ⋆ or □"]
  return ()

unPi :: Expr -> TcMonad (Bind Tele Expr)
unPi (Pi bnd) = return bnd
unPi e = throwError $ T.concat ["Expected pi type, got ", showExpr e, " instead"]

-- | alpha equality
checkEq :: Expr -> Expr -> TcMonad ()
checkEq e1 e2 =
  unless (aeq e1 e2) $ throwError $
    T.concat ["Couldn't match: ", showExpr e1, " with ", showExpr e2]

oneStep :: (MonadError T.Text m) => Expr -> m Expr
oneStep e = do
  case runFreshM . runMaybeT $ (step e) of
    Nothing -> throwError $ T.concat ["Cannot reduce ", showExpr e]
    Just e' -> return e'

-- | Contractiveness test
ctaTest :: Expr -> TmName -> TcMonad ()
ctaTest e x = do
  (t, _) <- lookupCtx
  case t of
    Prog  -> return ()
    Logic -> cta e

  where
    cta :: Expr -> TcMonad ()
    cta (Kind{}) = return ()
    cta (Var n)
      | n == x = throwError $ T.concat ["Contractiveness test failed: ", T.pack . show $ n]
      | otherwise = return ()
    cta (Pi{}) = return ()
    cta (App e1 e2) = mapM_ cta (e1 : [e2])
    cta (Lam bnd) = unbind bnd >>= cta . snd
    cta Nat = return ()
    cta (Lit{}) = return ()
    cta (PrimOp _ e1 e2) = cta e1 >> cta e2
