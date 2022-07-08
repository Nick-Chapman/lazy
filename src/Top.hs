
module Top (main) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  putStrLn "*lazy*"
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
  let sam = EApp double (EApp double (ENum 7))

  (res,prof) <- evalByGraphReduction sam
  print (res,prof)
  pure ()

data Exp = ENum Int | EAdd Exp Exp | EApp Exp Exp | ELam Id Exp | EVar Id

instance Show Exp where show = pretty

pretty :: Exp -> String
pretty = \case
  ENum n -> show n
  EAdd l r -> "(" ++ show l ++ "+" ++ show r ++ ")"
  EApp l r -> "(" ++ show l ++ " " ++ show r ++ ")"
  ELam x body -> "\\" ++ x ++ "." ++ show body
  EVar x -> x

type Id = String

data Value = VNum Int deriving Show

vadd :: Value -> Value -> Value
vadd v1 v2 = case (v1,v2) of (VNum n1, VNum n2) -> VNum (n1+n2)

evalByGraphReduction :: Exp -> IO (Value, Prof)
evalByGraphReduction exp = execG $ do
  nid <- loadExp exp
  evalToWHNF nid

loadExp :: Exp -> G Nid
loadExp = load
  where
    load :: Exp -> G Nid
    load = \case
      ENum n -> do
        Alloc (NNum n)
      EAdd left right -> do
        left <- load left
        right <- load right
        Alloc (NAdd left right)
      EApp func arg -> do
        func <- load func
        arg <- load arg
        Alloc (NApp func arg)
      ELam x body -> do
        body <- load body
        Alloc (NLam x body)
      EVar x -> do
        Alloc (NVar x)


unloadExp :: Nid -> G Exp
unloadExp = unload
  where
    unload :: Nid -> G Exp
    unload nid = do
      Fetch nid >>= \case
        NNum n -> do
          pure (ENum n)
        NAdd left right -> do
          left <- unload left
          right <- unload right
          pure (EAdd left right)
        NApp func arg -> do
          func <- unload func
          arg <- unload arg
          pure (EApp func arg)
        NLam x body -> do
          body <- unload body
          pure (ELam x body)
        NVar x -> do
          pure (EVar x)
        NIndirect target -> do
          target <- unload target
          pure (EApp (EVar "I") target)

evalToWHNF :: Nid -> G Value
evalToWHNF nid0 = do
  do e0 <- unloadExp nid0; Note ("evalToWHNF: " ++ show e0)
  eval nid0 []
  where
    eval :: Nid -> [(Nid,Nid)] -> G Value
    eval nid spine = do
      do e <- unloadExp nid; Note ("eval: " ++ show e)
      Fetch nid >>= \case
        NNum n -> do
          pure (VNum n)
        NAdd left right -> do
          left <- evalToWHNF left
          right <- evalToWHNF right
          TickAdd
          let res@(VNum n) = vadd left right
          --Debug ("add",left,right,"-->",res)
          Update nid (NNum n)
          pure res
        NApp func arg -> do
          eval func ((nid,arg) : spine)
        NLam binder body -> do
          case spine of
            [] -> error "evalToWHNF, functional result"
            (redux,arg):spine -> do
              -- do a beta reduction!!
              body' <- copySub (binder,arg) body
              Update redux (NIndirect body')
              TickBeta
              do e <- unloadExp redux; Note ("BETA: " ++ show e)
              eval redux spine
        NVar x -> do
          error (show ("evalToWHNF/var",x))
        NIndirect target -> do
          eval target spine


copySub :: (Id,Nid) -> Nid -> G Nid
copySub (binder,bound) = copy
  where
    copy :: Nid -> G Nid
    copy nid = do
      Fetch nid >>= \case
        NNum{} -> do
          pure nid -- dont copy; share
        NAdd left right -> do
          left <- copy left
          right <- copy right
          Alloc (NAdd left right)
        NApp func arg -> do
          func <- copy func
          arg <- copy arg
          Alloc (NApp func arg)
        NLam x body -> do
          if x == binder then pure nid else do -- dont copy under shadow
            body <- copy body
            Alloc (NLam x body)
        NVar x -> do
          pure $ if x == binder then bound else nid
        NIndirect{} -> do
          undefined -- TODO: provoke this with an example

instance Functor G where fmap = liftM
instance Applicative G where pure = return; (<*>) = ap
instance Monad G where return = Ret; (>>=) = Bind

data G a where
  Ret :: x -> G x
  Bind :: G x -> (x -> G y) -> G y
  Note :: String -> G ()
  Debug :: Show x => x -> G ()
  Alloc :: Node -> G Nid
  Fetch :: Nid -> G Node
  Update :: Nid -> Node -> G ()
  TickBeta :: G ()
  TickAdd :: G ()

data Node
  = NNum Int
  | NAdd Nid Nid
  | NApp Nid Nid
  | NLam Id Nid
  | NVar Id
  | NIndirect Nid
  deriving Show

data Nid = Nid Int
  deriving (Eq,Ord,Show)

execG :: G a -> IO (a,Prof)
execG g = loop state0 g $ \s a -> pure (a, prof s)
  where
    state0 = State { heap = Map.empty, u = 1, prof = prof0 }
    loop :: forall a b. State -> G a -> (State -> a -> IO b) -> IO b
    loop s g k = case g of
      Ret x -> k s x
      Bind g f -> loop s g $ \s a -> loop s (f a) k
      Note mes -> do
        putStrLn mes
        k s ()
      Debug x -> do
        print x
        k s ()
      Alloc node -> do
        let nid = Nid (u s)
        --print ("Alloc",node,"-->",nid)
        k s { heap = Map.insert nid node (heap s), u = u s + 1 } nid
      Fetch nid -> do
        let node = maybe (error (show ("Fetch",nid))) id $ Map.lookup nid (heap s)
        --print ("Fetch",nid,"-->",node)
        k s node
      Update nid node -> do
        --print ("Update",nid,node)
        case Map.lookup nid (heap s) of
          Nothing -> error "Update/Nothing"
          Just{} -> do
            k s { heap = Map.insert nid node (heap s) } ()
      TickBeta -> do
        k s { prof = (prof s) { beta = beta (prof s) + 1 }} ()
      TickAdd -> do
        k s { prof = (prof s) { add = add (prof s) + 1 }} ()


data State = State
  { heap :: Map Nid Node
  , u :: Int
  , prof :: Prof
  }

data Prof = Prof
  { beta :: Int
  , add :: Int
  } deriving Show

prof0 :: Prof
prof0 = Prof { beta = 0, add = 0 }
