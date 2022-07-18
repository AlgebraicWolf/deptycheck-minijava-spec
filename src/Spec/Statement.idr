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
    VarDeclaration : (type : JType) -> Statement n postV -> Statement (S n) (type::postV)
    Assignment : (k : Fin n)  -> (expr : Expression n postV (getType k postV)) -> (stmt : Statement n postV) -> So (isValidExpr expr stmt) -> Statement n postV
    Empty : Statement 0 []

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

