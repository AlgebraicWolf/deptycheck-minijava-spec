module Spec.Statement

import Data.Fin

import public Spec.Expression

-- This is based on MiniJava grammar
-- However, I've decided to add flexibility to the
-- generated programs by allowing the mixing of
-- 'usual' statements and variable declarations
public export
data Statement : (n, m : Nat) -> (preV : Variables n) -> (postV : Variables m) -> Type where
  VarDeclaration : (type : JType) -> Statement n (S n) vars (type::vars)
  Assignment : (k : Fin n)  => Expression n vars (getType k vars) -> Statement n n vars vars
  Compose : Statement n m pre mid -> Statement m k mid post -> Statement n k pre post

-- export
-- Show (Statement {n} {m} preV postV) where
--   show (VarDeclaration type) = show type ++ " x" ++ show m ++ ";\n"
--   show (Assignment n x) = show n ++ " = " ++ show x ++ ";\n"
--   show (Compose x y) = show x ++ show y

