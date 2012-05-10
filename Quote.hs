{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
module Quote 
       ( QOption(..)
       , proccess
       ) where

import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Enumerator as E hiding (map, span)
import qualified Data.Enumerator.List as EL
import Data.Time
import qualified Data.Sequence as S
import qualified Data.ListLike as LL
import Network.Pcap
import Network.Pcap.Enumerator

import Quote.Data
import Quote.Parser

data QOption = QOption
               { verbose :: Bool
               , reorder :: Bool
               , ports   :: [Int]
               , code    :: ByteString
               , pcap    :: FilePath
               }

proccess :: QOption -> IO ()
proccess opt = runStateT (run_ iter) S.empty >> return ()
  where 
    iter = enumOffline (pcap opt) $$ only opt =$ conv2Quote =$ printQ
    printQ = if reorder opt then printQuoteReorder else printQuote

only :: MonadIO m => QOption -> Enumeratee (PktHdr, ByteString) (PktHdr, ByteString) m b
only opt = EL.filter $ \(_, bs) ->
  let (dest, code') = portAndCode bs
  in (elem dest (ports opt)) && code' == code opt

conv2Quote :: MonadIO m => Enumeratee (PktHdr, ByteString) Quote m b
conv2Quote = EL.map $ uncurry parseFrame

type QHistory = S.Seq Quote
    
printQuote :: Iteratee Quote (StateT QHistory IO) ()
printQuote = do
  mq <- EL.head
  case mq of
    Nothing -> return ()
    Just q -> do
      liftIO $ print q
      printQuote

printQuoteReorder :: Iteratee Quote (StateT QHistory IO) ()
printQuoteReorder = do
  mq <- EL.head
  case mq of
    Nothing -> lift get >>= pr
    Just q -> do
      qs <- lift get
      let (!old, new) = flip LL.span qs $ \x ->
            (acceptTime x) <= addUTCTime (-3) (pktTime q)
          !new' = add new q
      lift $ put new'
      pr old
      printQuoteReorder
  where
    pr xs = liftIO $ LL.mapM_ print xs
    add v e | S.null v = S.singleton e
            | e >= LL.last v = LL.snoc v e
            | otherwise = LL.snoc (add (LL.init v) e) (LL.last v)
