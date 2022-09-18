module Spec.NameInitialized

import public Spec.Variables

%default total

public export
data NameInitialized : Nat -> Variables -> Type where
  Here : (prf : VariableDoesNotExist (MkVar name jty Init) vars) =>
         NameInitialized name ((MkVar name jty Init)::vars)
  There : (prf : VariableDoesNotExist var vars) =>
          NameInitialized name vars ->
          NameInitialized name (var::vars)
