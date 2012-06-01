{-# LANGUAGE FlexibleInstances, OverloadedStrings, MultiParamTypeClasses #-}
module Main (main) where
import Control.Monad
import Control.Exception (SomeException(..))

import qualified Data.ByteString.Char8 as S8
import Data.Monoid
import Data.Maybe

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute (mimeTypesI)
import Data.IterIO.SSL
import Data.IterIO.Server.TCPServer
import Data.IterIO.Http.Support

import System.IO.Unsafe
import System.Environment

import OpenSSL (withOpenSSL)

import Controllers
import Utils

main :: IO ()
main = withOpenSSL $ do
  env <- getEnvironment
  server <- case lookup "SSL_KEY_FILE" env of
             Nothing -> return simpleHttpServer
             Just f  -> simpleHttpsServer `liftM` simpleContext f
  let port = fromMaybe 8000 $ lookup "PORT" env >>= maybeRead :: Int
  runTCPServer $ server (fromIntegral port) handler

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
