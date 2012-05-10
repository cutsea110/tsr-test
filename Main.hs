{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Exception
import Data.Attoparsec.Char8
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.List (foldl')
import Data.Typeable
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStr, hPutStrLn, stderr)

import Quote

version :: String
version = "0.1.0"

data Options = Options
                 { optVerbose     :: Bool
                 , optShowVersion :: Bool
                 , optReorder     :: Bool
                 , optPort        :: [Int]
                 , optCode        :: String
                 } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optVerbose = False
  , optShowVersion = False
  , optReorder = False
  , optPort = []
  , optCode = ""
  }

optSpec :: [OptDescr (Options -> Options)]
optSpec =
  [ Option ['v'] ["verbose"]
    (NoArg (\opts -> opts {optVerbose = True}))
    "chatty output on stderr"
  , Option ['V','?'] ["version"]
    (NoArg (\opts -> opts {optShowVersion = True}))
    "show version number"
  , Option ['r'] ["reorder"]
    (NoArg (\opts -> opts {optReorder = True}))
    "reorder"
  , Option ['p'] ["port"]
    (ReqArg (\p opts -> opts {optPort = optPort opts ++ [readPort p]}) "PORT")
    "destination port number"
  , Option ['c'] ["code"]
    (ReqArg (\c opts -> opts {optCode = c}) "CODE")
    "quote target code like as \"B6034\""
  ]
  where
    readPort :: String -> Int
    readPort s = case feed (parse port (pack s)) BS.empty of
      Done r n  -> if BS.null r then n else throw err
      _         -> throw err
      where
        err = IllegalArg $ "\"" ++ s ++ "\" is illegal port number."
    port :: Parser Int
    port = do n <- many1 digit
              return $ read n
  
data QuoteError = NoSuchCommand String
                | CmdArg [String]
                | IllegalArg String
                | FileNotExist deriving (Show, Typeable)
instance Exception QuoteError
  
parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv = case getOpt Permute spec argv of
  (o,n,[]  ) -> (foldl' (flip id) defaultOptions o, n)
  (_,_,errs) -> throw (CmdArg errs)

showVersion :: IO ()
showVersion = hPutStrLn stderr version

main :: IO ()
main = flip catches handlers $ do
  args <- getArgs
  let (opt, cmdArg) = parseArgs optSpec args
  proccess $ QOption { verbose = optVerbose opt
                     , reorder = optReorder opt
                     , ports = optPort opt
                     , code = pack $ optCode opt
                     , pcap = if length cmdArg > 0 then head cmdArg else throw FileNotExist
                     }
  where
    handlers = [Handler handler0, Handler handler1]
    handler0 :: ErrorCall -> IO ()
    handler0 e = print e -- for debug
    handler1 :: QuoteError -> IO ()
    handler1 (NoSuchCommand cmd) = do
      hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
    handler1 (CmdArg errs) = do
      mapM_ (hPutStr stderr) errs
      printUsage
    handler1 (IllegalArg err) = do
      hPutStrLn stderr err
      printUsage
    handler1 FileNotExist = do
      hPutStrLn stderr $ "input pcap file not find"
      printUsage
    printUsage = hPutStrLn stderr $ "\n" ++ usageInfo usage optSpec
  
usage :: String
usage = "quote version " ++ version ++ "\n" ++ 
        "Usage:"
