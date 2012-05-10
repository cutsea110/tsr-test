module Quote.Data 
       ( Quote(..)
       , Q(..)
       )where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Time
import Data.List (intersperse)

data Quote = Quote { pktTime :: {-# UNPACK #-}!UTCTime
                   , acceptTime :: {-# UNPACK #-}!UTCTime
                   , issueCode :: {-# UNPACK #-}!ByteString
                   , bid5th :: {-# UNPACK #-}!Q
                   , bid4th :: {-# UNPACK #-}!Q
                   , bid3rd :: {-# UNPACK #-}!Q
                   , bid2nd :: {-# UNPACK #-}!Q
                   , bid1st :: {-# UNPACK #-}!Q
                   , ask1st :: {-# UNPACK #-}!Q
                   , ask2nd :: {-# UNPACK #-}!Q
                   , ask3rd :: {-# UNPACK #-}!Q
                   , ask4th :: {-# UNPACK #-}!Q
                   , ask5th :: {-# UNPACK #-}!Q
                   }
             
instance Show Q where
  show q = {-# SCC "showQ" #-}show (price q) ++ "@" ++ show (quantity q)

instance Show Quote where
  show q = let fs = {-# SCC "showQuote" #-}map show [ pktTime q, acceptTime q] ++ 
                    [unpack $ issueCode q] ++ 
                    map show [ bid5th q, bid4th q, bid3rd q, bid2nd q, bid1st q
                             , ask1st q, ask2nd q, ask3rd q, ask4th q, ask5th q]
         in concat $ intersperse " " fs

instance Eq Quote where
  q == q' = acceptTime q == acceptTime q' && pktTime q == pktTime q'

instance Ord Quote where
  compare = up compare
  (<) = up (<)
  (>=) = up (>=)
  (>) = up (>)
  (<=) = up (<=)

up :: (UTCTime -> UTCTime -> t) -> Quote -> Quote -> t
up op x y = if acceptTime x == acceptTime y
            then pktTime x `op` pktTime y
            else acceptTime x `op` acceptTime y
       
data Q = Q { price :: Integer
           , quantity :: Integer
           }
