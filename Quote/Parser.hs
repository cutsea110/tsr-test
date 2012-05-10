{-# LANGUAGE BangPatterns #-}
module Quote.Parser 
       ( portAndCode
       , parseFrame
       , parseQuote
       ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Attoparsec.Char8 as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.List as DL (foldl', take)
import Data.Time
import Data.Time.Clock.POSIX
import Network.Pcap

import Quote.Data

type Digit = Int

integer1 :: Parser Integer
integer1 = toInteger . c2d <$> digit
  where
    c2d x = ord x - ord '0'

integer :: Digit -> Parser Integer
integer d = DL.foldl' (liftM2 ((*10)>>>(+))) zero $ DL.take d $ repeat integer1
  where
    zero = return 0

bin1 :: Parser Int
bin1 = ord <$> anyChar

bin :: Digit -> Parser Int
bin d = DL.foldl' (liftM2 ((*256)>>>(+))) zero $ DL.take d $ repeat bin1
  where
    zero = return 0
  
pPort :: Parser Int
pPort = bin 2
pCode :: Parser ByteString
pCode = P.take 5
pIssueCode :: Parser ByteString
pIssueCode = P.take 12
pIssueSeqNo :: Parser Integer
pIssueSeqNo = integer 3
pMarketStatusType :: Parser ByteString
pMarketStatusType = P.take 2
pTotalQuoteVolume :: Parser Integer
pTotalQuoteVolume = integer 7
pPrice :: Parser Integer
pPrice = integer 5
pQuantity :: Parser Integer
pQuantity = integer 7
pQuote :: Parser Q
pQuote = do
  !p <- pPrice
  !q <- pQuantity
  return $ Q p q

numOfBestValidQuote :: Parser Integer
numOfBestValidQuote = integer 5
numOfBestQuote :: Parser Integer
numOfBestQuote = integer 4
pAcceptTime :: Parser TimeOfDay
pAcceptTime = do
  hh <- fmap fromInteger $ integer 2
  mm <- fmap fromInteger $ integer 2
  ss <- fmap fromInteger $ integer 2
  uu <- fmap fromInteger $ integer 2
  return $ TimeOfDay { todHour=hh, todMin=mm, todSec=(ss+uu/100) }

portAndCode :: ByteString -> (Int, ByteString)
portAndCode bs = (dest, cd)
  where
    (hdr, bdy) = BS.splitAt 42 bs
    Done _ dest = parse pPort $ BS.drop 36 hdr
    cd = BS.take 5 bdy

parseFrame :: PktHdr -> ByteString -> Quote
parseFrame ph bs = q
  where
    (_, bdy) = BS.splitAt 42 bs
    Done _ q = parse (parseQuote pTime) bdy
    pTime = toTime (hdrSeconds ph) (hdrUseconds ph)
    toTime s u = posixSecondsToUTCTime $ realToFrac s + (realToFrac u / (10^6))


parseQuote :: UTCTime -> Parser Quote
parseQuote ptime = do
  !cd <- pCode
  !ic <- pIssueCode
  !sn <- pIssueSeqNo
  !st <- pMarketStatusType
  !bqv <- pTotalQuoteVolume
  !b1 <- pQuote
  !b2 <- pQuote
  !b3 <- pQuote
  !b4 <- pQuote
  !b5 <- pQuote
  !aqv <- pTotalQuoteVolume
  !a1 <- pQuote
  !a2 <- pQuote
  !a3 <- pQuote
  !a4 <- pQuote
  !a5 <- pQuote
  !nbvq <- numOfBestValidQuote
  !nb1 <- numOfBestQuote
  !nb2 <- numOfBestQuote
  !nb3 <- numOfBestQuote
  !nb4 <- numOfBestQuote
  !nb5 <- numOfBestQuote
  !navq <- numOfBestValidQuote
  !na1 <- numOfBestQuote
  !na2 <- numOfBestQuote
  !na3 <- numOfBestQuote
  !na4 <- numOfBestQuote
  !na5 <- numOfBestQuote
  !at <- pAcceptTime
  return $ Quote { pktTime = ptime
                 , acceptTime = jstToUTC $ LocalTime { localDay = utctDay ptime
                                                     , localTimeOfDay = at }
                 , issueCode = ic
                 , bid5th = b5
                 , bid4th = b4
                 , bid3rd = b3
                 , bid2nd = b2
                 , bid1st = b1
                 , ask1st = a1
                 , ask2nd = a2
                 , ask3rd = a3
                 , ask4th = a4
                 , ask5th = a5
                 }

jstToUTC :: LocalTime -> UTCTime
jstToUTC = localTimeToUTC $ hoursToTimeZone 9
