module Spec.Statement

import Data.Fin

import public Spec.Expression

%default total

-- This is based on MiniJava grammar
-- However, I've decided to add flexibility to the
-- generated programs by allowing the mixing of
-- 'usual' statements and variable declarations

-- This was put in a separater structure to try
-- and break down the complexity of `Statement.Assignment` generation
public export
data AssignmentExpressionWrap : (name : Nat) -> (vars : Variables) -> Type where
  MkAssignmentExpressionWrap : (vars : Variables) ->
                               (name : Nat) ->
                               (jty : JType) ->
                               (expr : Expression vars jty) ->
                               ExistsOfType name jty vars ->
                               AssignmentExpressionWrap name vars

public export
data Statement : (vars : Variables) -> Type where
  VarDeclaration : (type : JType) ->
                   (name : Nat) ->
                   Statement vars ->
                   (prf : VariableDoesNotExist (MkVar name type NotInit) vars) =>
                   Statement ((MkVar name type NotInit)::vars)

  Assignment : (vars : Variables) ->
               -- Variable to assign to and its type
               (name : Nat) ->
               -- Continuation
               (stmt : Statement vars) ->
               -- Assignment stuff
               AssignmentExpressionWrap name vars ->
               -- Mark variable as initialized
               (newVars : Variables) ->
               Initialize name vars newVars =>
               Statement newVars

  Block : Statement varsInside ->
          (varsOutside : Variables) ->
          Statement vars -> -- continuation
          PrefixOf vars varsInside varsOutside =>
          Statement varsOutside

  Print : Expression vars jty ->
          Statement vars ->
          Statement vars

  Empty : Statement []

