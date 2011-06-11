module Main where

import System.Environment
import Data.Binary.Get
import Data.Word
import Data.Maybe
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Char
import QuotePacketParser
import Opts

data TsuruPacket = TsuruPacket { timestamp :: PcapTime
                               , packet    :: QuotePacket
                               }
    deriving Eq

instance Show TsuruPacket where
    show tp = show (timestamp tp) ++ " " ++ show (packet tp)

newtype PcapTime = PcapTime (Int,Int)
    deriving Eq

instance Show PcapTime where
    show (PcapTime (sec,usec)) = let (h,rs) = (sec `rem` 86400) `quotRem` 3600 -- hour and remaining seconds
                                     (m,s)  = rs   `quotRem` 60 -- minutes and seconds
                                     u      = usec `quot` 10000 -- def of usec
      in humanizeHour' $ Prelude.concatMap appendZero [h,m,s,u]

main :: IO ()
main = do
    o <- getArgs >>= compileOpts
    if containsInput o
      then do
        pcap <- BL.readFile $ getInput o
        if containsOrdered o
          then do
            print "Lets do it in order tomorrow..."
          else do
            forEachPacket print $ toStream pcap
      else do
        getProgName >>= printErrors []
    {- Skip the Pcap header -}
    where toStream = runGet $ (skip 24 >> getRemainingLazyByteString)

forEachPacket f bsi | BL.null bsi = return ()
                    | otherwise = do
                          let (mp,bs) = runGet parsePacket bsi
                          if mp == Nothing
                            then forEachPacket f bs
                            else do
                              f $ fromJust mp
                              forEachPacket f bs

parsePacket :: Get ((Maybe TsuruPacket), BL.ByteString)
parsePacket = do
    sec  <- getWord32host -- time sec
    usec <- getWord32host -- time usec
    len  <- getWord32host -- len received
    skip 4 -- skip the original len
    if len /= 257
      then do
        skip (fromIntegral len)
        liftA2 (,) (return Nothing) getRemainingLazyByteString
      else do
        {- We drop 12 bytes, of the Ethernet II packet - 6 from the destination addr
           and 6 more from the source addr. After this we have 2 bytes containing the
           type of the nest packet, 0x0800 (or "\b\NUL" in ASCII) is IP.
        -}
        skip 12
        proto <- getLazyByteString 2
        if proto /= ipCode
          then do
            skip (fromIntegral len - 14)
            liftA2 (,) (return Nothing) getRemainingLazyByteString
          else do
            skip 33
            (p,bs) <- parseQP
            if p == Nothing
              then return (Nothing,bs)
              else return (Just $ TsuruPacket (PcapTime (fromIntegral sec, fromIntegral usec)) $ fromJust p,bs)

ipCode  = BL.pack [8,0] -- code 0x0800
