module Spec.Statement

import Data.Fin

import public Spec.Expression

-- This is based on MiniJava grammar
-- However, I've decided to add flexibility to the
-- generated programs by allowing the mixing of
-- 'usual' statements and variable declarations
public export
data Statement : (n, m : Nat) -> (preV : Variables n) -> (postV : Variables m) -> Type where
  VarDeclaration : (type : JType) -> Statement n m preV postV -> Statement n (S m) preV (type::postV)
  Assignment : (k : Fin m)  => Expression m postV (getType k postV) -> Statement n m preV postV -> Statement n m preV postV
  Empty : Statement 0 0 [] []
-- This is bad due to a large number of symmetries
--  Compose : Statement n m pre mid -> Statement m k mid post -> Statement n k pre post
-- Thus, I've decided to move to a list-like structure to avoid having many different terms
-- encoding essentially the same program

-- export
-- Show (Statement {n} {m} preV postV) where
--   show (VarDeclaration type) = show type ++ " x" ++ show m ++ ";\n"
--   show (Assignment n x) = show n ++ " = " ++ show x ++ ";\n"
--   show (Compose x y) = show x ++ show y

