module Spec.Aspects.Variables.Quantifiers

import Spec.Aspects.Variables

-- public export
-- Non-polymorphic version of Any
-- data Any : (0 p : (Identifier, JType) -> Bool) -> Variables -> Type where
--   Here : {0 vars : Variables} -> So (p (id, jty)) -> Any p $ (id, jty)::vars
--   There : {0 vars : Variables} -> Any p vars -> Any p $ (id, jty)::vars

public export
data VarDefined : (n : Identifier) -> (vars : Variables) -> Type where
  Here :  VarDefined n $ (ident, jty)::vars
  There : VarDefined n vars -> VarDefined n $ (ident, jty)::vars
