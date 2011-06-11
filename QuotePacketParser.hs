module QuotePacketParser where

import Data.Time.LocalTime
import Data.Binary.Get
import Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.Time.Clock
import GHC.Int
import Control.Applicative

data QuotePacket = QuotePacket {
      --Data Type                               2   B6
      --Information Type                        2   03
      --Market Type                             1   4
      --Issue code                             12   ISIN code
      issueCode :: ByteString
      --Issue seq.-no.                          3
      --Market Status Type                      2
      --Total bid quote volume                  7
      --Best bid price(1st)                     5
    , bstBidPrc1 :: ByteString
      --Best bid quantity(1st)                  7
    , bstBidQt1 :: ByteString
      --Best bid price(2nd)                     5
    , bstBidPrc2 :: ByteString
      --Best bid quantity(2nd)                  7
    , bstBidQt2 :: ByteString
      --Best bid price(3rd)                     5
    , bstBidPrc3 :: ByteString
      --Best bid quantity(3rd)                  7
    , bstBidQt3 :: ByteString
      --Best bid price(4th)                     5
    , bstBidPrc4 :: ByteString
      --Best bid quantity(4th)                  7
    , bstBidQt4 :: ByteString
      --Best bid price(5th)                     5
    , bstBidPrc5 :: ByteString
      --Best bid quantity(5th)                  7
    , bstBidQt5 :: ByteString
      --Total ask quote volume                  7
      --Best ask price(1st)                     5
    , bstAskPrc1 :: ByteString
      --Best ask quantity(1st)                  7
    , bstAskQt1 :: ByteString
      --Best ask price(2nd)                     5
    , bstAskPrc2 :: ByteString
      --Best ask quantity(2nd)                  7
    , bstAskQt2 :: ByteString
      --Best ask price(3rd)                     5
    , bstAskPrc3 :: ByteString
      --Best ask quantity(3rd)                  7
    , bstAskQt3 :: ByteString
      --Best ask price(4th)                     5
    , bstAskPrc4 :: ByteString
      --Best ask quantity(4th)                  7
    , bstAskQt4 :: ByteString
      --Best ask price(5th)                     5
    , bstAskPrc5 :: ByteString
      --Best ask quantity(5th)                  7
    , bstAskQt5 :: ByteString
      --No. of best bid valid quote(total)      5
      --No. of best bid quote(1st)              4
      --No. of best bid quote(2nd)              4
      --No. of best bid quote(3rd)              4
      --No. of best bid quote(4th)              4
      --No. of best bid quote(5th)              4
      --No. of best ask valid quote(total)      5
      --No. of best ask quote(1st)              4
      --No. of best ask quote(2nd)              4
      --No. of best ask quote(3rd)              4
      --No. of best ask quote(4th)              4
      --No. of best ask quote(5th)              4
      -- *Quote accept time*                    8  HHMMSSuu
    , qat :: ByteString
      --End of Message                          1  0xff
    } deriving Eq

instance Show QuotePacket where
    show qp = (humanizeHour $ BC.unpack (qat qp)) ++ " " ++ BC.unpack (issueCode qp) ++ " " 
           
           ++ delZeros (bstBidQt5 qp) ++ "@" ++ delZeros (bstBidPrc5 qp) ++ " "
           ++ delZeros (bstBidQt4 qp) ++ "@" ++ delZeros (bstBidPrc4 qp) ++ " "
           ++ delZeros (bstBidQt3 qp) ++ "@" ++ delZeros (bstBidPrc3 qp) ++ " "
           ++ delZeros (bstBidQt2 qp) ++ "@" ++ delZeros (bstBidPrc2 qp) ++ " "
           ++ delZeros (bstBidQt1 qp) ++ "@" ++ delZeros (bstBidPrc1 qp) ++ " "

           ++ delZeros (bstAskQt1 qp) ++ "@" ++ delZeros (bstAskPrc1 qp) ++ " "
           ++ delZeros (bstAskQt2 qp) ++ "@" ++ delZeros (bstAskPrc2 qp) ++ " "
           ++ delZeros (bstAskQt3 qp) ++ "@" ++ delZeros (bstAskPrc3 qp) ++ " "
           ++ delZeros (bstAskQt4 qp) ++ "@" ++ delZeros (bstAskPrc4 qp) ++ " "
           ++ delZeros (bstAskQt5 qp) ++ "@" ++ delZeros (bstAskPrc5 qp)
        where delZeros = delZeros' . Prelude.dropWhile (=='0') . BC.unpack
              delZeros' s | Prelude.null s = "0"
                          | otherwise =  s

humanizeHour [t1,t2] = [t1,t2]
humanizeHour (t1:t2:xs) = [t1 , t2 , ':' ] ++ humanizeHour xs
appendZero n = (if n<10 then "0" else "") ++ show n

--parseQP :: Get (Maybe QuotePacket)
parseQP = do
    issueCode <- getBytes 12
    skip 12
    bstBidPrc1 <- getBytes 5
    bstBidQt1 <- getBytes 7
    bstBidPrc2 <- getBytes 5
    bstBidQt2 <- getBytes 7
    bstBidPrc3 <- getBytes 5
    bstBidQt3 <- getBytes 7
    bstBidPrc4 <- getBytes 5
    bstBidQt4 <- getBytes 7
    bstBidPrc5 <- getBytes 5
    bstBidQt5 <- getBytes 7
    skip 7
    bstAskPrc1 <- getBytes 5
    bstAskQt1 <- getBytes 7
    bstAskPrc2 <- getBytes 5
    bstAskQt2 <- getBytes 7
    bstAskPrc3 <- getBytes 5
    bstAskQt3 <- getBytes 7
    bstAskPrc4 <- getBytes 5
    bstAskQt4 <- getBytes 7
    bstAskPrc5 <- getBytes 5
    bstAskQt5 <- getBytes 7
    skip 50
    qat <- getBytes 8
    eop <- getWord8
    if eop /= 0xff
      then do
        liftA2 (,) (return Nothing) getRemainingLazyByteString
      else do
        bs <- getRemainingLazyByteString
        return $ (Just $ QuotePacket issueCode bstBidPrc1 bstBidQt1
                         bstBidPrc2 bstBidQt2 bstBidPrc3 bstBidQt3 bstBidPrc4 bstBidQt4 bstBidPrc5 bstBidQt5
                         bstAskPrc1 bstAskQt1 bstAskPrc2 bstAskQt2 bstAskPrc3 bstAskQt3 bstAskPrc4 bstAskQt4
                         bstAskPrc5 bstAskQt5 qat,bs)
