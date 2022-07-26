module Spec.Statement

import Data.Fin

import public Spec.Expression

-- This is based on MiniJava grammar
-- However, I've decided to add flexibility to the
-- generated programs by allowing the mixing of
-- 'usual' statements and variable declarations
mutual
  public export
  data Statement : (n : Nat) -> (postV : Variables n) -> Type where
    VarDeclaration : (type : JType) ->
                     Statement n postV ->
                     Statement (S n) (type::postV)
    Assignment : (k : Fin n) ->
                 (expr : Expression n postV (getType k postV)) ->
                 (stmt : Statement n postV) ->
                 ExprValid n postV stmt (getType k postV) expr ->
                 Statement n postV
    Empty : Statement 0 []


  public export
  data VariableInitialized : (n : Nat) -> (vars : Variables n) -> (k : Fin n) -> (stmt : Statement n vars) -> Type where
    InitHere : VariableInitialized n vars k (Assignment k expr stmt prf)
    InitThereVD : VariableInitialized n vars k stmt ->
                  VariableInitialized (S n) (_::vars) (FS k) (VarDeclaration jty stmt)
    InitThereAssignment : VariableInitialized n vars k stmt ->
                          VariableInitialized n vars k (Assignment m expr stmt prf)

  public export
  data ExprValid : (n : Nat) ->
                   (vars : Variables n) ->
                   (stmt : Statement n vars) ->
                   (res : JType) ->
                   (expr : Expression n vars res) ->
                   Type where
    TrueValid : ExprValid n vars stmt _ BoolTrue
    FalseValid : ExprValid n vars stmt _ BoolFalse
    IntValid : ExprValid n vars stmt _ (IntegerLiteral x)
    InitValid : VariableInitialized n vars k stmt ->
                (prf : IsOfType n k (getType k vars) vars) ->
                ExprValid n vars stmt _ (FromIdentifier @{prf} k)

  public export
  isInitialized : {n : Nat} -> {vars : Variables n} -> (k : Fin n) -> Statement n vars -> Bool
  isInitialized FZ (VarDeclaration type stmt) = False
  isInitialized (FS y) (VarDeclaration type stmt) = isInitialized y stmt
  isInitialized k (Assignment x expr stmt prf) = if k == x then True else isInitialized k stmt
  isInitialized k Empty impossible

  public export
  isValidExpr : {n : Nat} -> {vars : Variables n} -> {res : JType} -> Expression n vars res -> Statement n vars -> Bool
  isValidExpr (FromIdentifier k) stmt = isInitialized k stmt
  isValidExpr _ _ = True -- Very very dangerous, don't forget to edit

