{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Control.Monad.Trans
import Control.Exception (SomeException(..))

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute (mimeTypesI, runHttpRoute, routeName)
import Data.IterIO.Server.TCPServer
import Data.IterIO.Http.Support

import System.IO.Unsafe

import Controllers

type L = L8.ByteString
type S = S8.ByteString

main :: IO ()
main = runTCPServer $ simpleHttpServer 8000 handler

handler :: HttpRequestHandler IO ()
handler = runIterAction $ runActionRoute $ mconcat
  [ routeTop $ routeAction $ restIndex UsersController
  , routeRestController "users" UsersController
  , routeMethod "GET"  $ routePattern "login" $ routeAction newLoginUser
  , routeMethod "POST" $ routePattern "login" $ routeAction loginUser
  , routeMethod "GET" $ routePattern "logout" $ routeAction logoutUser
  , routeFileSys mimeMap "static"
  ]


-- | Given a file extension (e.g., \"hs\") return its MIME type (e.g.,
-- \"text\/x-haskell\"). If there is no recognized MIME type (or none
-- of the default paths exist), this function returns
-- \"application\/octet-stream\"
mimeMap :: String -> S8.ByteString
mimeMap = unsafePerformIO $ 
  foldr1 cat (map enumMimeFile defaultPaths) |$
    mimeTypesI "application/octet-stream"
    where defaultPaths = ["mime.types"
                         , "/etc/mime.types"
                         , "/var/www/conf/mime.types"]
          enumMimeFile f = inumCatch (enumFile f) $ \(SomeException _) res ->
                                                      resumeI res
