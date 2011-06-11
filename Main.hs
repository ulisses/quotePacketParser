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
import qualified Data.MeldableHeap.Lazy as LHeap

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
      in humanizeHour $ Prelude.concatMap appendZero [h,m,s,u]

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

{- This function guarantee that the we are in the presence of "the packet we want".
   This packet must:
       - Have 257 bytes of length (14 for eth, 20 for IP, 8 for UDP and 215 of DATA)
       - Be IP (here we discard ARP and others with format: eth:XXX)
       - Be UDP (here we discard PIMv2, STP, NBS and protocols like that, with format: eth:ip:XXX)
       - Be a packet to UDP dst port 15515 or 15516
       - Be a Quote packet inside UDP, this means, must have "B6034" in the beginning of DATA
       - Terminate with 0xff

   I realize that the rule (==257 bytes) is enough if we use the "mdf-kospi200.20110216-0.pcap"
   example file. But for the sake of safety we will check even more to be completely sure
   that this packet matches the above rules. We are also using Lazy Binay.Get so we won't
   have any parsing error if we fail, so if better if we just predict all the cases.
-}
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
            {- We drop 9 bytes and we reach the protocol type field inside IP packet.
               If this 1 byte field is 17, we have UDP inside.
            -}
            skip 9
            prot <- getLazyByteString 1
            if prot /= udpCode
              then do
                skip (fromIntegral len - 24)
                liftA2 (,) (return Nothing) getRemainingLazyByteString
              else do
              {- We drop the rest of the IP packet (10 bytes) and 2 bytes from the begining
                 of the UDP packet. We are interested in byte 3 and 4, that have the dst port
              -}
              skip 12
              dstPort <- getBytes 2
              if dstPort /= _15515 && dstPort /= _15516
                then do
                  skip (fromIntegral len - 38)
                  liftA2 (,) (return Nothing) getRemainingLazyByteString
                else do
                  skip 4 -- drop the rest of the udp packet
                  code <- getBytes 5
                  if code /= qcode
                    then do
                      skip (fromIntegral len - 47)
                      liftA2 (,) (return Nothing) getRemainingLazyByteString
                    else do
                    (p,bs) <- parseQP
                    if p == Nothing
                      then return (Nothing,bs)
                      else return (Just $ TsuruPacket (PcapTime (fromIntegral sec, fromIntegral usec)) $ fromJust p,bs)

ipCode  = BL.pack [8,0] -- code 0x0800
udpCode = BL.pack [17]  -- code 17
_15515  = BC.pack "<\155"
_15516  = BC.pack "<\156"
qcode   = BC.pack "B6034"
