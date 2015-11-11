{-# LANGUAGE OverloadedStrings #-}

module BaseTransJava (compile) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Text as T
import qualified Language.Java.Syntax as J
import           Lens.Micro
import           Unbound.Generics.LocallyNameless

import           Environment
import           JavaLang
import           StringPrefix
import           Syntax
import           TypeCheck

type TransJavaExp = Either J.Name J.Literal

type TransType = ([J.BlockStmt], TransJavaExp, Expr)

type Translate = TcMonad


compile :: Expr -> (Either T.Text J.CompilationUnit)
compile = runTcMonad dummyCtx . createWrap

createWrap :: Expr -> Translate J.CompilationUnit
createWrap e = do
  (bs, e, t) <- trans e
  let returnStmt = [bStmt $ J.Return $ Just (unwrap e)]
  let returnType = applyRetType t
  let javaCode = wrapperClass False "Test" (bs ++ returnStmt) returnType mainBody
  return (createCUB Nothing [javaCode])

applyRetType Nat = Just $ J.PrimType J.IntT
applyRetType _ = Just objClassTy

createCUB :: Maybe J.PackageDecl -> [J.TypeDecl] -> J.CompilationUnit
createCUB package compDef = cu
  where
    cu = J.CompilationUnit package [] compDef



-- | Translation to Java
trans :: Expr -> Translate TransType
trans e =
  case e of
    Var n -> do
      t <- fst <$> lookupTy n
      return ([], var (show n), t)

    Lam bnd -> do
      (s, je, t) <- translateScopeM bnd Nothing
      return (s, je, Pi t)

    App e1 e2 -> transApply e1 e2

    Lit lit -> return ([], Right . J.Int . fromIntegral $ lit, Nat)

    PrimOp op e1 e2 -> do
      (s1, j1, _) <- trans e1
      (s2, j2, _) <- trans e2
      let j1' = unwrap j1
      let j2' = unwrap j2
      let (je, t) =
            case op of
              Add  -> (J.BinOp j1' J.Add j2', Nat)
              Mult -> (J.BinOp j1' J.Mult j2', Nat)
              Sub  -> (J.BinOp j1' J.Sub j2', Nat)
      newName <- fresh (string2Name "prim" :: TmName)
      let assignExpr = localFinalVar (javaType t) (varDecl (show newName) je)
      return (s1 ++ s2 ++ [assignExpr], var (show newName), t)

    Let bnd -> do
      ((x, Embed e), b) <- unbind bnd
      let x' = name2String x
      (s1, j1, t1) <- trans e
      -- types do need to generate code
      if (aeq t1 estar || aeq t1 ebox)
        then trans (subst x e b)
        else do
          let jt1 = javaType t1
          let xDecl = localVar jt1 (varDecl x' $ unwrap j1)
          (s2, j2, t2) <- extendCtx (mkTele [(x', t1)]) (trans b)
          return (s1 ++ [xDecl] ++ s2, j2, t2)

    Nat -> do
      (js, v) <- createTypeHouse "nat"
      return (js, v, Kind Star)

    Kind Star -> do
      (js, v) <- createTypeHouse "star"
      return (js, v, Kind Box)

    Pi _ -> do
      (js, v) <- createTypeHouse "pi"
      return (js, v, Kind Star)

    _ -> throwError "Not implemented yet"


{-

(\(x1:t1)..) e

==>

class Fx1 extends Closure {
<..>
}
Closure cx1 = new Fx1();

cx1.arg = j2;
cx1.apply();
|retTy| rx1 = |retTy| cx1.res;


-}
transApply :: Expr -> Expr -> Translate TransType
transApply e1 e2 = do
  (s1, j1, Pi bnd) <- trans e1
  (d@(Cons de), b) <- unbind bnd
  let ((x1, _, _), de') = unrebind de
  (s2, j2, _) <- trans e2
  let (d', b') = multiSubst d e2 b
  let retTy =
          case d' of
            Empty -> b'
            _     -> Pi (bind d' b')

  let fexp = unwrap j1
  let fs = assignField (fieldAccExp fexp closureInput) (unwrap j2)
  let fapply = bStmt . applyMethodCall $ fexp
  let fout = fieldAccess fexp closureOutput
  let fret = "r" ++ show x1
  let fres = [initClass (javaType retTy) fret fout]

  return (s1 ++ s2 ++ (fs : fapply : fres), var fret, retTy)



translateScopeM :: Bind Tele Expr -> Maybe TmName -> Translate ([J.BlockStmt], TransJavaExp, Bind Tele Expr)
translateScopeM bnd n = do
  (delta, m) <- unbind bnd
  (bodyBS, bodyV, bodyT) <- extendCtx delta (trans m)

  (closureBS, cx1) <- translateScopeTy delta (bodyBS, bodyV, bodyT) n

  return (closureBS, cx1, bind delta bodyT)

{-

\(x1 : t1)(..) . e

==>

class Fcx1 extends Closure {

  Closure cx1 = this;

  public void apply() {
    final |t1| x1 = cx1.arg;
    <...>
  }
}
Closure cx1 = new Fcx1();

-}
translateScopeTy :: Tele -> TransType -> Maybe TmName -> Translate ([J.BlockStmt], TransJavaExp)
translateScopeTy b (ostmts, oexpr, t1) n = translateScopeTyp' b
  where
    translateScopeTyp' (Cons bnd) = do
      let ((x1, _, Embed t1), b) = unrebind bnd

      let cx1 = maybe ("c" ++ show x1) (show . id) n -- FIXME: use prefix

      let accessField = fieldAccess (left $ var cx1) closureInput
      let xf = initClass (javaType t1) (show x1) accessField

      (body', xe) <- case b of
                       Empty -> return (ostmts, oexpr)
                       Cons b' ->
                         translateScopeTyp' b

      let fstmt = localVar closureType (varDecl cx1 (funInstCreate cx1))

      return
        ([ localClassDecl (closureTransName ++ cx1) closureClass
             (closureBodyGen [memberDecl $ fieldDecl (classTy closureClass) (varDecl cx1 J.This)]
                ([xf] ++ body' ++ [bsAssign (name [closureOutput]) (unwrap xe)]))
         , fstmt
         ], var cx1)



javaType ty =
  case ty of
    Pi _ -> classTy closureClass
    Nat  -> classTy "java.lang.Integer"
    _    -> objClassTy

initClass :: J.Type -> String -> J.Exp -> J.BlockStmt
initClass ty tempName expr =
  localFinalVar ty
    (varDecl tempName
       (if ty == objClassTy
          then expr
          else cast ty expr))


createTypeHouse :: (Fresh m) => String -> m ([J.BlockStmt], TransJavaExp)
createTypeHouse str = do
  typeVar <- show <$> fresh (s2n str)
  let fstmt = [ localVar typeOfType (varDecl typeVar typeHouseCreate)
              , assignField (fieldAccExp (left . var $ typeVar) typeField) (J.Lit (J.String str))
              ]
  return (fstmt, var typeVar)
