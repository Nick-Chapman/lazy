
module Top (main) where

import Lang (Exp(..))
import System.Environment (getArgs)
import qualified Example as X

import qualified GraphEval as G
import qualified DirectEval as D

main :: IO ()
main = do
  putStrLn "*lazy*"
  args <- getArgs
  let config = parseConfig args
  run config

data Config = Config { arg :: Int }

parseConfig :: [String] -> Config
parseConfig = \case
  [n] -> Config $ read n
  [] -> Config 0
  args -> error (show ("config",args))

run :: Config -> IO ()
run Config{arg} = do
  let _ = arg
  --runX "sam" $ X.sam
  --runX "thrice-thrice" $ X.thriceExample
  --runX "scott" $ EApp X.convScott (X.makeScott arg)
  runX "listExample" $ X.listExample arg

runX :: String -> Exp -> IO ()
runX tag exp = do
  let _ = do
        (res,prof,u) <- G.evalByGraphReduction exp
        print ("G",tag,res,prof,u)
  do
    (res,prof,u) <- D.evalDirect exp
    print ("D",tag,"result=",res,"prof=",prof,"allocs=",u)
  pure ()
