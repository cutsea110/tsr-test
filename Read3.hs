{-# LANGUAGE OverloadedStrings #-}
module Read3 where

import Numeric (showHex)
import System.Time
import Control.Monad (when)
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
  let (dest, bdy) = parse bs
  when (elem dest destIPs) (act bdy)
  where
    pktTime = (toTime (hdrSeconds ph) (hdrUseconds ph))
    toTime s u = posixSecondsToUTCTime $ realToFrac s + (realToFrac u / (10^6))
    act bdy = do
      print pktTime
      print bdy

type Port = Word8

toHex :: Word8 -> String
toHex b = (if b < 16 then ('0':) else id) (showHex b "")

destIPs :: [Port]
destIPs = [15515, 15516]

spec :: [Int]
spec = [ 2 -- Data Type
       , 2 -- Information Type
       , 1 -- Market Type
       , 12 -- Issue code
       , 3 -- Issue seq.-no.
       , 2 -- Market Status Type
       , 7 -- Total bid quote volume
       , 5 -- Best bid price(1)
       , 7 -- Best bid quantity(1)
       , 5 -- Best bid price(2)
       , 7 -- Best bid quantity(2)
       , 5 -- Best bid price(3)
       , 7 -- Best bid quantity(3)
       , 5 -- Best bid price(4)
       , 7 -- Best bid quantity(4)
       , 5 -- Best bid price(5)
       , 7 -- Best bid quantity(5)
       , 7 -- Total ask quote volume
       , 5 -- Best ask price(1)
       , 7 -- Best ask quantity(1)
       , 5 -- Best ask price(2)
       , 7 -- Best ask quantity(2)
       , 5 -- Best ask price(3)
       , 7 -- Best ask quantity(3)
       , 5 -- Best ask price(4)
       , 7 -- Best ask quantity(4)
       , 5 -- Best ask price(5)
       , 7 -- Best ask quantity(5)
       , 5 -- No. of best bid valid quote(total)
       , 4 -- No. of best bid quote(1)
       , 4 -- No. of best bid quote(2)
       , 4 -- No. of best bid quote(3)
       , 4 -- No. of best bid quote(4)
       , 4 -- No. of best bid quote(5)
       , 5 -- No. of best ask valid quote(total)
       , 4 -- No. of best ask quote(1)
       , 4 -- No. of best ask quote(2)
       , 4 -- No. of best ask quote(3)
       , 4 -- No. of best ask quote(4)
       , 4 -- No. of best ask quote(5)
       , 8 -- *Quote accept time* HHMMSSuu
       , 1 -- End of Message(0xff)
       ]

parse :: ByteString -> (Port, ByteString)
parse bs = (dest, bdy)
  where
    (hdr, bdy) = BS.splitAt 42 bs
    dest = toIP $ BS.take 2 $ BS.drop 36 hdr
    toIP b2 = let [h,l] = BS.unpack b2 in h*256+l
