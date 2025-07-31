module Daylatro.Type.Flag where

import qualified System.Console.GetOpt as GetOpt

data Flag
  = BaseUrl String
  | Database String
  | Help
  | Host String
  | Port String
  | Version
  deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
  [ GetOpt.Option ['h'] ["help"] (GetOpt.NoArg Help) "",
    GetOpt.Option [] ["version"] (GetOpt.NoArg Version) "",
    GetOpt.Option [] ["base-url"] (GetOpt.ReqArg BaseUrl "URL") "",
    GetOpt.Option [] ["database"] (GetOpt.ReqArg Database "STRING") "",
    GetOpt.Option [] ["host"] (GetOpt.ReqArg Host "STRING") "",
    GetOpt.Option [] ["port"] (GetOpt.ReqArg Port "INT") ""
  ]
