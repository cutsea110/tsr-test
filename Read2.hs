{-# LANGUAGE OverloadedStrings #-}
module Read2 where

import Numeric (showHex)
import System.Time
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS -- read instance definition only
import Data.ByteString.Char8 ()
import Foreign
import Network.Pcap

main :: IO ()
main = do
  p <- openOffline "mdf-kospi200.20110216-0.pcap"
  loopBS p (-1) printItBS
  return ()

printItBS :: PktHdr -> ByteString -> IO ()
printItBS ph bs = do
  print $ toTime (hdrSeconds ph) (hdrUseconds ph)
  print bs
  where
    toTime s u = posixSecondsToUTCTime $ realToFrac s + (realToFrac u / (10^6))

toHex :: Word8 -> String
toHex b = (if b < 16 then ('0':) else id) (showHex b "")
