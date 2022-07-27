module Spec.Statement

import Data.Fin

import public Spec.Expression

-- This is based on MiniJava grammar
-- However, I've decided to add flexibility to the
-- generated programs by allowing the mixing of
-- 'usual' statements and variable declarations
public export
data Statement : (n : Nat) -> (vars : Variables n) -> Type where
  VarDeclaration : (type : JType) ->
                   Statement n vars ->
                   Statement (S n) ((MkVar type False)::vars)
  Assignment : (k : Fin n) ->
               (expr : Expression n vars (getType k vars)) ->
               (stmt : Statement n vars) ->
               Statement n (makeInit k vars)
  Empty : Statement 0 []

