module Spec.Initialize

import public Spec.Variables

import Decidable.Equality

%default total

public export
data Initialize : Nat -> Variables -> Variables -> Type where
  Here : (prf  : VariableDoesNotExist (MkVar name jty init) vars) =>
         (prf' : VariableDoesNotExist (MkVar name jty Init) vars) =>
         Initialize name ((MkVar name jty init)::vars) ((MkVar name jty Init)::vars)
  There : (prf  : VariableDoesNotExist var oldVars) =>
          (prf' : VariableDoesNotExist var newVars) =>
          Initialize name oldVars newVars ->
          Initialize name (var :: oldVars) (var :: newVars)

