module QuotePacketParser where

import Text.Parsec
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec.Char
import Data.Binary.Get

data QuotePacket = QuotePacket {
      --Data Type                               2   B6
    dataType :: Int
      --Information Type                        2   03
    , infoType :: Int
      --Market Type                             1   4
    , mrktType :: Int
      --Issue code                             12   ISIN code
    , issueCode :: Int
      --Issue seq.-no.                          3
    , issueSeq :: Int
      --Market Status Type                      2
    , mrktStatus :: Int
      --Total bid quote volume                  7
    , totBidVol :: Int
      --Best bid price(1st)                     5
    , bstBidPrc1 :: Int
      --Best bid quantity(1st)                  7
    , bstBidQt1 :: Int
      --Best bid price(2nd)                     5
    , bstBidPrc2 :: Int
      --Best bid quantity(2nd)                  7
    , bstBidQt2 :: Int
      --Best bid price(3rd)                     5
    , bstBidPrc3 :: Int
      --Best bid quantity(3rd)                  7
    , bstBidQt3 :: Int
      --Best bid price(4th)                     5
    , bstBidPrc4 :: Int
      --Best bid quantity(4th)                  7
    , bstBidQt4 :: Int
      --Best bid price(5th)                     5
    , bstBidPrc5 :: Int
      --Best bid quantity(5th)                  7
    , bstBidQt5 :: Int
      --Total ask quote volume                  7
    , totAsk :: Int
      --Best ask price(1st)                     5
    , bstAskPrc1 :: Int
      --Best ask quantity(1st)                  7
    , bstAskQt1 :: Int
      --Best ask price(2nd)                     5
    , bstAskPrc2 :: Int
      --Best ask quantity(2nd)                  7
    , bstAskQt2 :: Int
      --Best ask price(3rd)                     5
    , bstAskPrc3 :: Int
      --Best ask quantity(3rd)                  7
    , bstAskQt3 :: Int
      --Best ask price(4th)                     5
    , bstAskPrc4 :: Int
      --Best ask quantity(4th)                  7
    , bstAskQt4 :: Int
      --Best ask price(5th)                     5
    , bstAskPrc5 :: Int
      --Best ask quantity(5th)                  7
    , bstAskQt5 :: Int
      --No. of best bid valid quote(total)      5
    , nrBstBid :: Int
      --No. of best bid quote(1st)              4
    , nrBstBid1 :: Int
      --No. of best bid quote(2nd)              4
    , nrBstBid2 :: Int
      --No. of best bid quote(3rd)              4
    , nrBstBid3 :: Int
      --No. of best bid quote(4th)              4
    , nrBstBid4 :: Int
      --No. of best bid quote(5th)              4
    , nrBstBid5 :: Int
      --No. of best ask valid quote(total)      5
    , nrBstAsk :: Int
      --No. of best ask quote(1st)              4
    , nrBstQuote1 :: Int
      --No. of best ask quote(2nd)              4
    , nrBstQuote2 :: Int
      --No. of best ask quote(3rd)              4
    , nrBstQuote3 :: Int
      --No. of best ask quote(4th)              4
    , nrBstQuote4 :: Int
      --No. of best ask quote(5th)              4
    , nrBstQuote5 :: Int
      -- *Quote accept time*                     8  HHMMSSuu
    , qat :: TimeZone
      --End of Message                          1  0xff
    } deriving (Eq)


example :: String
example = "B6034KR4201F4292806211000199600023000002300022000000000021000010000020000000000019000000000008930003100000040003200000000003300000000003400000000003500000000001100020000000100000000001090001000000000000000009000000\255"

parseQP :: Get QuotePacket
parseQP = do
    dataType <- getByteString 2
    infoType <- getByteString 2
    mrktType <- getByteString 1
    issueCode <- getByteString 12
    issueSeq  <- getByteString 3
    mrktStatus <- getByteString 2
    totBidVol <- getByteString 7
    bstBidPrc1 <- getByteString 5
    bstBidQt1 <- getByteString 7
    bstBidPrc2 <- getByteString 5
    bstBidQt2 <- getByteString 7
    bstBidPrc3 <- getByteString 5
    bstBidQt3 <- getByteString 7
    bstBidPrc4 <- getByteString 5
    bstBidQt4 <- getByteString 7
    bstBidPrc5 <- getByteString 5
    bstBidQt5 <- getByteString 7
    totAsk <- getByteString 7
    bstAskPrc1 <- getByteString 5
    bstAskQt1 <- getByteString 7
    bstAskPrc2 <- getByteString 5
    bstAskQt2 <- getByteString 7
    bstAskPrc3 <- getByteString 5
    bstAskQt3 <- getByteString 7
    bstAskPrc4 <- getByteString 5
    bstAskQt4 <- getByteString 7
    bstAskPrc5 <- getByteString 5
    bstAskQt5 <- getByteString 7
    nrBstBid <- getByteString 5
    nrBstBid1 <- getByteString 4
    nrBstBid2 <- getByteString 4
    nrBstBid3 <- getByteString 4
    nrBstBid4 <- getByteString 4
    nrBstBid5 <- getByteString 4
    nrBstAsk <- getByteString 5
    nrBstQuote1 <- getByteString 4
    nrBstQuote2 <- getByteString 4
    nrBstQuote3 <- getByteString 4
    nrBstQuote4 <- getByteString 4
    nrBstQuote5 <- getByteString 4
    qat <- getByteString 8
    _ <- getByteString 1
    return $ QuotePacket dataType
























