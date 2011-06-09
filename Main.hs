module Main where

import Network.Pcap
import qualified Data.ByteString as BL
import GHC.Word
import Data.Binary.Get
import Data.Char

import QuotePacketParser

main :: IO ()
main = do
    p <- openOffline "mdf-kospi200.20110216-0.pcap"
    process p

--process :: PcapHandle  -> IO()
process ph = do
    p@(hdr,bs) <- nextBS ph
    if BL.null bs then do return() else do
        if isThePacketWeWant p
            then do
                print $ getQuotePacket bs
                process ph
            else
                process ph

{- This function guarantee that the we are in the presence of "the packet we want".
   This packet must:
       - Have 257 bytes of length (14 for eth, 20 for IP, 8 for UDP and 215 of DATA)
       - Be UDP (here we discard ARP, PIMv2, STP, NBS and protocols like that)
       - Be a packet to UDP dst port 15515 or 15516
       - Be a Quote packet inside UDP, this means, must have "B6034" in the beginning of DATA
-}
isThePacketWeWant :: (PktHdr, BL.ByteString) -> Bool
isThePacketWeWant (hdr,bs) = hdrCaptureLength hdr == 257
                          && isUDP bs
                          && isDstUDPPort bs
                          && isQuotePacket bs

getQuotePacket :: BL.ByteString -> BL.ByteString
getQuotePacket = dropUDPPacket

isQuotePacket :: BL.ByteString -> Bool
isQuotePacket bs = let quote = BL.take 5 $ dropUDPPacket bs
                       magicBs = BL.pack $ map (fromIntegral . fromEnum) "B6034"
                   in quote == magicBs

isDstUDPPort :: BL.ByteString -> Bool
isDstUDPPort bs = let _15515 = BL.pack [141,203] --port 15515
                      _15516 = BL.pack [141,206] --port 15516
                      bytes = BL.take 2 $ dropIPv4Packet bs
                  in bytes == _15515 || bytes == _15516

dropUDPPacket,dropIPv4Packet,dropEthPacket :: BL.ByteString -> BL.ByteString
dropUDPPacket = BL.drop 8 . dropIPv4Packet
dropIPv4Packet = BL.drop 20 . dropEthPacket
dropEthPacket = BL.drop 14

{- We drop 12 bytes, of the Ethernet II packet - 6 for the destination addr
   and 6 more for the source addr. After that we have 2 bytes containing the
   type of the nest packet, 0x0800 is UDP
-}
isUDP :: BL.ByteString -> Bool
isUDP bs = let field = BL.take 2 $ BL.drop 12 bs
           in  field == BL.pack [8,0]

--printIt :: PktHdr -> Ptr Word8 -> IO ()
printIt ph bs = do
    print ph
    print bs

