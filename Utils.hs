{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (listToMaybe, fromJust)

import Data.IterIO.Http (reqCookies)
import Data.IterIO.Http.Support
import Data.Bson (genObjectId)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.IterIO.Http (respAddHeader)


-- | Force get parameter value
getParamVal :: Monad m => S8.ByteString -> Action t b m String
getParamVal n = (L8.unpack . paramValue . fromJust) `liftM` param n

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

with404orJust :: Monad m => Maybe a -> (a -> Action t b m ()) -> Action t b m ()
with404orJust mval act = case mval of
                           Nothing -> respond404
                           Just val -> act val

-- | Redirect  back if the referer header is set, otherwise to the
-- given URL
redirectBackOrTo :: String -> Action t b IO ()
redirectBackOrTo url = do
  mhdr <- requestHeader (S8.pack "referer")
  redirectTo $ maybe url S8.unpack mhdr

-- | Redirect to the set refer, if set; or given URL.
redirectToSavedRefererOrTo :: String -> Action t b IO ()
redirectToSavedRefererOrTo url = do
  req <- getHttpReq
  let mref = lookup "_hails_referer" $ reqCookies req
  redirectTo $ maybe url S8.unpack mref
  delCookie "_hails_referer"


--
-- Flash notifications
--

-- | This sets the @_flash-*@ cookie value to the given message, with
-- a unique message ID.
flash :: String -> String -> Action t b IO ()
flash n msg = do
  oid <- liftIO genObjectId
  setCookie ("_flash-" ++ n) (show (show oid ++ "|" ++ msg))

flashInfo :: String -> Action t b IO ()
flashInfo = flash "info"

flashError :: String -> Action t b IO ()
flashError = flash "error"

flashSuccess :: String -> Action t b IO ()
flashSuccess = flash "success"

setCookie :: String -> String -> Action t b IO ()
setCookie n v = modify $ \s ->
    let cHeader = ( S8.pack "Set-Cookie"
                  , S8.pack $ n ++ "=" ++ v ++ ";path=/;")
    in s { actionResp = respAddHeader cHeader (actionResp s)}

delCookie :: String -> Action t b IO ()
delCookie n = modify $ \s ->
  let cHeader = ( S8.pack "Set-Cookie", S8.pack $
                  n ++ "=; path=/; expires=Thu, Jan 01 1970 00:00:00 UTC;")
  in s { actionResp = respAddHeader cHeader (actionResp s)}
