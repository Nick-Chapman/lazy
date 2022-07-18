
module DirectEval
  ( evalDirect
  , Value(..), Prof(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Lang --(Exp(..),Id)
import qualified Data.Map as Map

data Value = VNum Int | VFunc Id Exp Env
  deriving Show

vadd :: Value -> Value -> Value
vadd v1 v2 = case (v1,v2) of
  (VNum n1, VNum n2) -> VNum (n1+n2)
  _ -> error "vadd"

type Env = Map Id Loc

env0 :: Env
env0 = Map.empty

data Thunk = Thunk Exp Env | AlreadyValue Value
  deriving Show

evalDirect :: Exp -> IO (Value, Prof,Int)
evalDirect exp = execH $ do
  eval env0 exp

eval :: Env -> Exp -> H Value
eval q exp = do
 Note ("eval: " ++ show exp ++ ", env=" ++ show q)
 case exp of
  ENum n -> do
    pure $ VNum n
  EAdd left right -> do
    v1 <- eval q left
    v2 <- eval q right
    let res = vadd v1 v2
    TickAdd
    Note (show ("add",v1,v2,"-->",res))
    pure res
  EApp func arg -> do
    arg <- makeThunk q arg
    func <- eval q func
    apply func arg
  ELam x body -> do
    pure $ VFunc x body q
  EVar x -> do
    case Map.lookup x q of
      Nothing -> error (show ("eval/EVar",x))
      Just loc -> force loc
  EFix body -> do
    undefined body

makeThunk :: Env -> Exp -> H Loc
makeThunk env = \case
  EVar x -> do
    case Map.lookup x env of
      Nothing -> error (show ("makeThunk/EVar",x))
      Just loc -> pure loc
  exp -> do
    Alloc (Thunk exp env)


force :: Loc -> H Value
force loc = Fetch loc >>= \case
  AlreadyValue value -> pure value
  Thunk exp env -> do
    value <- eval env exp
    Update loc (AlreadyValue value)
    pure value

apply :: Value -> Loc -> H Value
apply func arg = do
 Note ("apply: " ++ show func ++ " @ " ++ show arg)
 case func of
  VNum{} -> error "apply/num"
  VFunc x body env -> do
    TickBeta
    let env' = Map.insert x arg env
    eval env' body


instance Functor H where fmap = liftM
instance Applicative H where pure = return; (<*>) = ap
instance Monad H where return = Ret; (>>=) = Bind

data H a where
  Ret :: x -> H x
  Bind :: H x -> (x -> H y) -> H y
  Note :: String -> H ()
  TickBeta :: H ()
  TickAdd :: H ()
  Alloc :: Thunk -> H Loc
  Fetch :: Loc -> H Thunk
  Update :: Loc -> Thunk -> H ()

data Loc = Loc Int
  deriving (Eq,Ord,Show)

execH :: H a -> IO (a,Prof,Int)
execH g = loop state0 g $ \s a -> pure (a, prof s, u s)
  where
    state0 = State { heap = Map.empty, u = 0, prof = prof0 }
    loop :: forall a b. State -> H a -> (State -> a -> IO b) -> IO b
    loop s g k = case g of
      Ret x -> k s x
      Bind g f -> loop s g $ \s a -> loop s (f a) k
      Note mes -> do
        putStrLn mes
        k s ()
      TickBeta -> do
        k s { prof = (prof s) { beta = beta (prof s) + 1 }} ()
      TickAdd -> do
        k s { prof = (prof s) { add = add (prof s) + 1 }} ()
      Alloc thunk -> do
        let u' = u s + 1
        let loc = Loc u'
        print ("Alloc",thunk,"-->",loc)
        k s { heap = Map.insert loc thunk (heap s), u = u' } loc
      Fetch loc -> do
        let thunk = maybe (error (show ("Fetch",loc))) id $ Map.lookup loc (heap s)
        print ("Fetch",loc,"-->",thunk)
        k s thunk
      Update loc thunk -> do
        print ("Update",loc,thunk)
        case Map.lookup loc (heap s) of
          Nothing -> error "Update/Nothing"
          Just{} -> do
            k s { heap = Map.insert loc thunk (heap s) } ()

data State = State
  { heap :: Map Loc Thunk
  , u :: Int
  , prof :: Prof
  }

data Prof = Prof
  { beta :: Int
  , add :: Int
  } deriving Show

prof0 :: Prof
prof0 = Prof { beta = 0, add = 0 }
