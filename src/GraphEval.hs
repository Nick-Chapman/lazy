
module GraphEval
  ( evalByGraphReduction
  , Value(..), Prof(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Lang (Exp(..),Id)
import qualified Data.Map as Map

data Value = VNum Int deriving Show

vadd :: Value -> Value -> Value
vadd v1 v2 = case (v1,v2) of (VNum n1, VNum n2) -> VNum (n1+n2)

evalByGraphReduction :: Exp -> IO (Value, Prof,Int)
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
      EFix body -> do
        body <- load body
        --Alloc (NFix body)
        res <- Alloc NBlackHole
        Update res (NApp body res)
        pure res

_unloadExp :: Nid -> G Exp
_unloadExp = unload
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
        {-NFix body -> do
          body <- unload body
          pure (EFix body)-}
        NIndirect target -> do
          target <- unload target
          pure (EApp (EVar "I") target)
        NBlackHole{} -> do
          undefined

evalToWHNF :: Nid -> G Value
evalToWHNF nid0 = do
  --do e0 <- unloadExp nid0; Note ("evalToWHNF: " ++ show e0)
  eval nid0 []
  where
    eval :: Nid -> [(Nid,Nid)] -> G Value
    eval me spine = do
      --do e <- unloadExp me; Note ("eval: " ++ show e)
      Fetch me >>= \case
        NNum n -> do
          pure (VNum n)
        NAdd left right -> do
          left <- evalToWHNF left
          right <- evalToWHNF right
          TickAdd
          let res@(VNum n) = vadd left right
          --Debug ("add",left,right,"-->",res)
          Update me (NNum n)
          pure res
        NApp func arg -> do
          eval func ((me,arg) : spine)
        NLam binder body -> do
          case spine of
            [] -> error "evalToWHNF, functional result"
            (redux,arg):spine -> do
              -- do a beta reduction!!
              body' <- copySub (binder,arg) body
              Update redux (NIndirect body')
              TickBeta
              --do e <- unloadExp redux; Note ("BETA: " ++ show e)
              eval redux spine
        NVar x -> do
          error (show ("evalToWHNF/var",x))
        {-NFix func -> do
          -- EFix e ==> EApp e (EFix e)
          -- implement fixpoint by unrolling one step.
          unrolled <- Alloc (NApp func me)
          eval unrolled spine-}
        NIndirect target -> do
          eval target spine
        NBlackHole{} -> do
          undefined

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
        {-NFix body -> do
          body <- copy body
          Alloc (NFix body)-}
        NIndirect body -> do
          --undefined -- TODO: provoke this with an example
          body <- copy body
          Alloc (NIndirect body)
        NBlackHole{} -> do
          undefined

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
  -- | NFix Nid
  | NBlackHole
  deriving Show

data Nid = Nid Int
  deriving (Eq,Ord,Show)

execG :: G a -> IO (a,Prof,Int)
execG g = loop state0 g $ \s a -> pure (a, prof s, u s)
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
