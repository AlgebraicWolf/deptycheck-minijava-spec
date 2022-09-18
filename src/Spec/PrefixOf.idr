module Spec.PrefixOf

import public Spec.Variables

%default total

-- Proof that one list of variables is formed by extending another list of variables
-- While proving, construct a new variable list that will be used outside the block
public export
data PrefixOf : Variables -> Variables -> Variables -> Type where
  -- Empty ctx
  Empty : PrefixOf [] [] []
  -- Initialize inside the block
  InitInside : (prf   : NameDoesNotExist nm vars) =>
               (prf'  : NameDoesNotExist nm varsInside) =>
               (prf'' : NameDoesNotExist nm varsOutside) =>
               PrefixOf vars varsInside varsOutside ->
               PrefixOf ((MkVar nm jty NotInit)::vars) ((MkVar nm jty Init)::varsInside) ((MkVar nm jty Init)::varsOutside)
  -- Don't change
  DontChange : (prf   : VariableDoesNotExist var vars) =>
               (prf'  : VariableDoesNotExist var varsInside) =>
               (prf'' : VariableDoesNotExist var varsOutside) =>
               PrefixOf vars varsInside varsOutside ->
               PrefixOf (var::vars) (var::varsInside) (var::varsOutside)
  -- Declare variable in the inner scope
  DeclNew : (prf : VariableDoesNotExist var varsInside) =>
             PrefixOf vars varsInside varsOutside ->
             PrefixOf vars (var :: varsInside) varsOutside


