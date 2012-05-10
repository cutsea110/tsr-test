module Test where

import Char
import Control.Monad.State

getInputNum :: Int -> IO Int
getInputNum n = do
  putStr $ "Type a number(" ++ show n ++ ")> "
  input <- getLine
  if (not.null $ filter isDigit input) then
    return $ digitToInt $ head $ filter (isDigit) input
    else fail "Quit"

compute :: Int -> StateT Int IO Int
compute a = do
  x <- get
  i <- lift (getInputNum x)
  put (x+1)
  return (i*(a+x))

testM :: IO ()
testM = do
  evalStateT (mapM compute [1..5]) 1 >>= putStrLn . show

testCatch = catch testM catcher
  where
    catcher :: IOError -> IO ()
    catcher e | e == (userError "Quit") = putStrLn "Quittting.."
              | otherwise = ioError e

main = testCatch