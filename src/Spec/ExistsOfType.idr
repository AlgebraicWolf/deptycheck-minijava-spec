module Spec.ExistsOfType

import public Spec.Variables

%default total

-- Proof that there exists variable of certain type with certain name
public export
data ExistsOfType : (name : Nat) -> (jty : JType) -> (vars : Variables) -> Type where
  Here : (prf : NameDoesNotExist name vars) =>
          ExistsOfType name jty ((MkVar name jty init)::vars)
  There : ExistsOfType name jty vars ->
           (prf : VariableDoesNotExist var vars) =>
           ExistsOfType name jty (var::vars)

