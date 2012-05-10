module Read1 where

import Numeric (showHex)
import System.Time
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Network.Pcap.Base

main :: IO ()
main = do
  p <- openOffline "mdf-kospi200.20110216-0.pcap"
  withForeignPtr p $ \ptr ->
    loop ptr (-1) printIt
  return ()

printIt :: PktHdr -> Ptr Word8 -> IO ()
printIt ph bytep = do
  d <- peekArray (fromIntegral (hdrCaptureLength ph)) bytep
  print $ toTime (hdrSeconds ph) (hdrUseconds ph)
  print $ map toHex d
  where
    toTime s u = posixSecondsToUTCTime $ realToFrac s + (realToFrac u / (10^6))
    


toHex :: Word8 -> String
toHex b = (if b < 16 then ('0':) else id) (showHex b "")
