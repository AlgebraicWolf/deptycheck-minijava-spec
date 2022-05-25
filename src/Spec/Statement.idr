module Spec.Statement

import Data.Fin

import public Spec.Expression

-- This is based on MiniJava grammar
-- However, I've decided to add flexibility to the
-- generated programs by allowing the mixing of
-- 'usual' statements and variable declarations
public export
data Statement : {n, m : Nat} -> (preV : Variables n) -> (postV : Variables m) -> Type where
  VarDeclaration : (type : JType) -> Statement vars (type::vars)
  Assignment : (k : Fin n)  => Expression n vars (getType k vars) -> Statement vars vars
  Compose : Statement pre mid -> Statement mid post -> Statement pre post

-- export
-- Show (Statement {n} {m} preV postV) where
--   show (VarDeclaration type) = show type ++ " x" ++ show m ++ ";\n"
--   show (Assignment n x) = show n ++ " = " ++ show x ++ ";\n"
--   show (Compose x y) = show x ++ show y

