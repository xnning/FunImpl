{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module Environment (
    lookupTy,
    extendCtx,
    runTcMonad,
    TcMonad,
    initialEnv,
    multiSubst,
    teleToEnv
    ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.Text as T
import           Lens.Micro
import           Lens.Micro.TH
import           Unbound.Generics.LocallyNameless

import           Syntax

type Env = [(TmName, Expr)]
data Context = Ctx {_env :: Env}

makeLenses ''Context

type TcMonad = FreshMT (ReaderT Context (Except T.Text))


runTcMonad :: Context -> TcMonad a -> (Either T.Text a)
runTcMonad env m = runExcept $ runReaderT (runFreshMT m) env

initialEnv :: Context
initialEnv = Ctx []

lookupTy :: (MonadReader Context m, MonadError T.Text m) => TmName -> m Expr
lookupTy v = do
  ctx <- asks _env
  case lookup v ctx of
    Nothing  -> throwError $ T.concat ["Not in scope: ", T.pack . show $ v]
    Just res -> return res

extendCtx :: (MonadReader Context m) => [(TmName, Expr)] -> m a -> m a
extendCtx d = local (over env (++ d))

multiSubst :: Tele -> Expr -> Expr -> (Tele, Expr)
multiSubst Empty _ e = (Empty, e)
multiSubst (Cons rb) t e = (b', e')
  where
    ((x, _), b) = unrebind rb
    e' = subst x t e
    b' = subst x t b

teleToEnv :: Tele -> Env
teleToEnv Empty  = []
teleToEnv (Cons rb) = (x, t) : teleToEnv b
  where
    ((x, Embed t), b) = unrebind rb
