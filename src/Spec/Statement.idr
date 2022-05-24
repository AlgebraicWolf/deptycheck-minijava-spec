module Spec.Statement

import Data.List.Lookup

import public Spec.Expression

-- This is based on MiniJava grammar
-- However, I've decided to add flexibility to the
-- generated programs by allowing the mixing of
-- 'usual' statements and variable declarations
public export
data Statement : (preV : Variables) -> (postV : Variables) -> Type where
  VarDeclaration : (type : JType) -> (n : Identifier) -> (0 newPrf : Uninhabited (Lookup n vars)) => Statement vars ((n, type)::vars)
  Assignment : (n : Identifier) -> (0 lk : Lookup n vars) => Expression vars (lk.reveal) -> Statement vars vars
  Compose : Statement pre mid -> Statement mid post -> Statement pre post

export
Show (Statement preV postV) where
  show (VarDeclaration type n) = show type ++ " " ++ show n ++ ";\n"
  show (Assignment n x) = show n ++ " = " ++ show x ++ ";\n"
  show (Compose x y) = show x ++ show y

