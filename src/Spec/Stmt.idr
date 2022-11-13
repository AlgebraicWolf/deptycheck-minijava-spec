module Spec.Stmt

import public Spec.Expression
import public Spec.ExistsOfType
import public Spec.Initialize
import public Spec.PrefixOf

%default total

public export
data AssignmentExpressionWrap : (name : Nat) -> (vars : Variables) -> Type where
  MkAssignmentExpressionWrap : (vars : Variables) ->
                               (name : Nat) ->
                               (jty : JType) ->
                               (expr : Expression vars jty) ->
                               ExistsOfType name jty vars ->
                               AssignmentExpressionWrap name vars

public export
data Stmt : (preV : Variables) -> (postV : Variables) -> Type where
  VarDeclaration : (type : JType) ->
                   (name : Nat) ->
                   (prf : NameDoesNotExist name preV) =>
                   Stmt preV ((MkVar name type NotInit)::preV)

  Assignment : (name : Nat) ->
               AssignmentExpressionWrap name vars ->
               (newVars : Variables) ->
               Initialize name vars newVars =>
               Stmt vars newVars

  Print : Expression vars jty ->
          Stmt vars vars
  -- TODO Print
