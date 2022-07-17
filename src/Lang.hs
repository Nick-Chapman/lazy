
module Lang (Exp(..), Id) where

data Exp
  = ENum Int
  | EAdd Exp Exp
  | EApp Exp Exp
  | ELam Id Exp
  | EVar Id
  | EFix Exp

instance Show Exp where show = pretty

pretty :: Exp -> String
pretty = \case
  ENum n -> show n
  EAdd l r -> "(" ++ show l ++ "+" ++ show r ++ ")"
  EApp l r -> "(" ++ show l ++ " " ++ show r ++ ")"
  ELam x body -> "\\" ++ x ++ "." ++ show body
  EVar x -> x
  EFix body -> "(fix:" ++ show body ++ ")"

type Id = String
