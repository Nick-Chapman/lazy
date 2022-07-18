
module Example
  ( sam
  , thriceExample
  , makeScott, convScott
  ) where

import Lang

sam :: Exp
sam = do
  let x = EVar "x"
  let f = EVar "f"
  let n = EVar "n"
  let inc = ELam "n" (EAdd n (ENum 1))
  let id = ELam "x" x
  let double = ELam "x" $ EAdd x x
  let twice = ELam "f" $ ELam "x" $ EApp f (EApp f x)
  let thrice = ELam "f" $ ELam "x" $ EApp f (EApp f (EApp f x))
  let _sam = ENum 42
  let _sam = EAdd (ENum 42) (EAdd (ENum 1) (ENum 2))
  let _sam = EApp inc (EApp inc (ENum 42))
  let _sam = EApp id (ENum 42)
  let _sam = EApp (EApp id id) (ENum 42)
  let _sam = EApp (EApp (EApp id id) (EApp id id)) (ENum 42)
  let _sam = EApp inc (EApp inc (ENum 5))
  let _sam = EApp (EApp twice inc) (ENum 100)
  let _sam = EApp (EApp thrice inc) (ENum 100)
  let _sam = EApp (EApp thrice (EApp thrice inc)) (ENum 100)
  let _sam = EApp (EApp (EApp thrice thrice) inc) (ENum 100)
  let _sam = EApp double (EApp double (ENum 7))
  let k42 = ELam "ignored" $ ENum 42
  let _sam = EApp k42 (EApp double (ENum 7))

  let sam = EApp (ELam "x" $ EApp double (EVar "x")) (EAdd (ENum 1) (ENum 2))
  
  sam

thriceExample :: Exp
thriceExample = do
  EApp (EApp (EApp thrice thrice) inc) (ENum 100)
  where
    inc = ELam "n" (EAdd n (ENum 1))
    thrice = ELam "f" $ ELam "x" $ EApp f (EApp f (EApp f x))
    x = EVar "x"
    f = EVar "f"
    n = EVar "n"

-- Scott numerals
makeScott :: Int -> Exp
makeScott = \case
  0 -> zero
  n -> EApp succ (makeScott (n-1))
  where
    zero = ELam "z" $ ELam "s" $ EVar "z"
    succ = ELam "n" $ ELam "z" $ ELam "s" $ EApp (EVar "s") (EVar "n")

convScott :: Exp
convScott = do
  -- convert Scott numeral to builtin number, using fixpoint
  EFix $ ELam "conv" $ ELam "n" $ EApp (EApp (EVar "n") z) s
    where z = ENum 0
          s = ELam "x" $ EAdd (ENum 1) (EApp (EVar "conv") (EVar "x"))
