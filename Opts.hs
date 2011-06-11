module Opts where

import System.Environment
import System.Console.GetOpt

data Options = OutOrdered | Input String
    deriving Eq

options =
    [ Option "r" [] (NoArg OutOrdered)    "Order and print messages by quote accept time"
    , Option "i" [] (ReqArg Input "FILE") "Input FILE in pcap format"
    ]

compileOpts :: [String] -> IO [Options]
compileOpts argv = do
    name <- getProgName
    case getOpt Permute options argv of
       (o,_,[])   -> do
           return o
       (_,_,errs) -> printErrors errs name

printErrors errs name = ioError $ userError $ concat errs ++ usageInfo header options
    where header = "Usage: "++ name ++" [OPTION...] <FILE>"

getInput = getInput' . head . filter containsInput'
getInput' (Input i) = i

containsInput = or . map containsInput'
containsInput' (Input _) = True
containsInput' _ = False

containsOrdered = Prelude.any (==OutOrdered)

getNameProg = getProgName
