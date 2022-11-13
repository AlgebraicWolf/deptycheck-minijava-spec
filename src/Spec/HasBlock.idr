module Spec.HasBlock

import public Spec.Class

public export
data StatementHasBlock : (preV : Variables) -> (postV : Variables) -> Statement preV postV -> Type where
  Here :  PrefixOf postV postV_inside postV =>
          (inside : Statement postV postV_inside) ->
          StatementHasBlock preV postV (InnerBlock cont inside postV)

  There : StatementHasBlock preV postV stmt ->
          StatementHasBlock preV postV' (Stmt stmt instr)

public export
data StatementWithBlock : (vars : Variables) -> Type where
  MkStatementWithBlock : (stmt : Statement preV postV) -> StatementHasBlock preV postV stmt -> StatementWithBlock vars

-- data MainClassHasBlock : MainClass -> Type where
--   BasedOnStmtWithBlock : StatementHasBlock vars stmt -> MainClassHasBlock (MkMain nm stmt)

-- data ProgramHasBlock : Program -> Type where
--   BasedOnMainClassWithBlock : MainClassHasBlock class -> ProgramHasBlock (MkProgram class)

-- public export
-- data ProgramWithBlock : Type where
  -- MkProgWithBlock : (prog : Program) -> ProgramHasBlock prog
