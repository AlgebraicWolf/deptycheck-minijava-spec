module Spec.Statement

import Data.Fin

import public Spec.Expression

%default total

-- This is based on MiniJava grammar
-- However, I've decided to add flexibility to the
-- generated programs by allowing the mixing of
-- 'usual' statements and variable declarations

public export
data Statement : (vars : Variables) -> Type where
  VarDeclaration : (type : JType) ->
                   Statement vars ->
                   Statement ((MkVar type False)::vars)
  Assignment : (vars : Variables) -> (k : Fin $ length vars) ->
               (expr : Expression vars (getType vars k)) ->
               (stmt : Statement vars) ->
               Statement (makeInit vars k)
  Empty : Statement []

