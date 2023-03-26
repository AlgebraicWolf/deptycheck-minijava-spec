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
data ExistsOfTypeAndInitialize : (name : Nat) -> (jty : JType) -> (vars : Variables) -> (newVars : Variables) -> Type where
  HereHere : (prf : NameDoesNotExist name vars) =>
             ExistsOfTypeAndInitialize name jty (MkVar name jty init :: vars) (MkVar name jty Init :: vars)
  ThereThere :(prf  : VariableDoesNotExist var oldVars) =>
              (prf' : VariableDoesNotExist var newVars) =>
              ExistsOfTypeAndInitialize name jty oldVars newVars ->
              ExistsOfTypeAndInitialize name jty (var :: oldVars) (var :: newVars)

-- from : ExistsOfTypeAndInitialize nm jty vars newVars -> (ExistsOfType nm jty vars, Initialize nm vars newVars)
-- from HereHere = (Here, Here)
-- from (ThereThere x) = let (a, b) = from x in (There a, There b)

-- existsNotImp : ExistsOfType nm jty vars -> NameDoesNotExist nm vars -> Void
-- existsNotImp Here (DeclDiff x (DiffName (NotRefl f))) = f Refl
-- existsNotImp (There x) (DeclDiff y z) = existsNotImp x y

-- to : ExistsOfType nm jty vars -> Initialize nm vars newVars -> ExistsOfTypeAndInitialize nm jty vars newVars
-- to (Here @{prf}) (Here @{NameAvailable prf} @{prf'})= HereHere {name=nm} {jty=jty} @{prf}
-- to Here (There x) = ?what_3
-- to (There x @{(NameAvailable y)}) Here = absurd $ existsNotImp x y
-- to (There x) (There y) = ThereThere $ to x y

public export
data Stmt : (preV : Variables) -> (postV : Variables) -> Type where
  VarDeclaration : (type : JType) ->
                   (name : Nat) ->
                   (prf : NameDoesNotExist name preV) =>
                   Stmt preV ((MkVar name type NotInit)::preV)

  Assignment : (name : Nat) ->
               (vars : Variables) ->
               (newVars : Variables) ->
               (jty : JType) ->
               (expr : Expression vars jty) ->
               ExistsOfTypeAndInitialize name jty vars newVars =>
               Stmt vars newVars

  Print : Expression vars jty ->
          Stmt vars vars
  -- TODO Print
