module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Attoparsec.Char8 hiding (take)
import qualified Data.Attoparsec.Char8 as AC (take)
import Data.Char (ord)
import Data.List (foldl')
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString (ByteString)

int :: Parser Integer
int = toInteger . c2d <$> digit
  where
    c2d x = ord x - ord '0'

int2 :: Parser Integer
int2 = liftM2 ((*10)>>>(+)) int int

type Digit = Int
integer :: Digit -> Parser Integer
integer d = foldl' (liftM2 ((*10)>>>(+))) zero $ take d $ repeat int
  where
    zero = return 0

integer' :: Digit -> Parser Integer
integer' d = read . unpack <$> AC.take d

test :: Int -> [Result Integer]
test n = map (\(l,bs) -> parse (integer l) bs) (genData n)

test' :: Int -> [Result Integer]
test' n = map (\(l,bs) -> parse (integer' l) bs) (genData n)

genData :: Int -> [(Int, ByteString)]
genData n = take n $ map (length &&& pack) $ splits primes stream
  where
    stream = cycle ['0'..'9']
    primes = cycle [2,3,5,7,11,13,17,19,23,29,31]

splits :: [Int] -> String -> [String]
splits []     _ = []
splits (n:ns) s = h:splits ns t
  where
    (h, t) = splitAt n s

main :: IO ()
main = print $ test 100000
