
module Top (main) where

import Lang (Exp(..))
import System.Environment (getArgs)
import qualified Example as X

import qualified GraphEval as G

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
  runG "thrice-thrice" $ X.thriceExample
  runG "scott" $ EApp X.convScott (X.makeScott arg)

runG :: String -> Exp -> IO ()
runG tag exp = do
  (res,prof,u) <- G.evalByGraphReduction exp
  print (tag,res,prof,u)
  pure ()
