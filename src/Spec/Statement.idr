module Spec.Statement

import Data.Fin

import public Spec.Expression

%default total

-- This is based on MiniJava grammar
-- However, I've decided to add flexibility to the
-- generated programs by allowing the mixing of
-- 'usual' statements and variable declarations

public export
data Statement : (vars : Variables) -> (init : InitializedVariables) -> Type where
  VarDeclaration : (type : JType) ->
                   (name : Nat) ->
                   Statement vars init ->
                   (prf : VariableDoesNotExist (MkVar name type) vars) =>
                   Statement ((MkVar name type)::vars) init

  Assignment : (vars : Variables) ->
               (name : Nat) ->
               (jty : JType) ->
               (expr : Expression vars init jty) ->
               (stmt : Statement vars init) ->
               ExistsOfType name jty vars =>
               Statement vars (name::init)

  Print : Expression vars init jty ->
          Statement vars init ->
          Statement vars init

  Empty : Statement [] []

