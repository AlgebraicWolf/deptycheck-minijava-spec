module Spec.HasBlock

import public Spec.Class

public export
data StatementHasBlock : (vars : Variables) -> Statement vars -> Type where
  Here : (inside : Statement varsInside) ->
         (cont : Statement vars) ->
         (prf : PrefixOf vars varsInside varsOutside) =>
         StatementHasBlock varsOutside (Block inside varsOutside cont)
  AppendVarDecl :
    (prf : VariableDoesNotExist (MkVar name type NotInit) vars) =>
    StatementHasBlock vars cont ->
    StatementHasBlock ((MkVar name type NotInit)::vars) (VarDeclaration type name cont)

  AppendAssignment : StatementHasBlock vars cont ->
                     (prf : Initialize name vars newVars) =>
                     StatementHasBlock newVars (Assignment vars name cont wrap newVars)

  AppendPrint : StatementHasBlock vars cont ->
                StatementHasBlock vars (Print expr cont)


public export
data StatementWithBlock : (vars : Variables) -> Type where
  MkStatementWithBlock : (stmt : Statement vars) -> StatementHasBlock vars stmt -> StatementWithBlock vars

-- data MainClassHasBlock : MainClass -> Type where
--   BasedOnStmtWithBlock : StatementHasBlock vars stmt -> MainClassHasBlock (MkMain nm stmt)

-- data ProgramHasBlock : Program -> Type where
--   BasedOnMainClassWithBlock : MainClassHasBlock class -> ProgramHasBlock (MkProgram class)

-- public export
-- data ProgramWithBlock : Type where
  -- MkProgWithBlock : (prog : Program) -> ProgramHasBlock prog
