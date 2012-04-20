{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Control.Monad.Trans
import Control.Exception (SomeException(..))

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid
import Data.Maybe
import Data.Digest.Pure.SHA

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute (mimeTypesI, runHttpRoute, routeName)
import Data.IterIO.Server.TCPServer
import Data.IterIO.Http.Support
import Control.Monad

import Data.Bson (ObjectId)
import Database.MongoDB.Structured (noSObjId)

import Crypto.BCrypt

import Layouts
import Models
import Views
import Utils

import Text.Regex.Posix

import System.Environment

type L = L8.ByteString
type S = S8.ByteString

data UsersController = UsersController

instance RestController t L IO UsersController where
  restIndex _ = do
    euser <- getCurrentUser (renderHtml $ loginView Nothing)
    either id (redirectBackOrTo . ("/users/" ++)) euser

  restShow _ _ = do
    euser <- getCurrentUser (redirectBackOrTo "/")
    either id (renderHtml .  showView) euser

  restNew _ = renderHtml $ newUser Nothing

  restCreate _ = parseParams >> do
    env <- liftIO getEnvironment
    let m_hmac_key   = lookup "HMAC_KEY" env
        mdomain      = lookup "COOKIE_DOMAIN" env
    paramOrBack "user_name"                   $ \uName ->
      paramOrBack "email"                     $ \email ->
        paramOrBack "password"                $ \pass0 ->
          paramOrBack "password_confirmation" $ \pass1 -> do
            let usr = User { userId       = noSObjId
                           , userName     = uName
                           , userEmail    = email
                           , userPassword = pass0 }
            exists <- liftIO $ userExists uName
            case () of
              _ | blank uName || blank email || blank pass0 -> 
                mkEdit usr >> flashError "Fields cannot be blank."

              _ | pass0 /= pass1 ->
                mkEdit usr >> flashError "Passwords do not match."

              _ | not (wellformed uName) -> do
                mkEdit $ usr { userName = "" }
                flashError $ "Username must start with a letter and only"
                           ++ " contain letters, numbers and underscore."

              _ | exists -> mkEdit usr >> flashError "Username taken"

              _ | isNothing m_hmac_key -> respondStat stat500

              _ -> do mpass <- liftIO $ bcrypt pass0
                      case mpass of
                        Nothing -> respondStat stat500
                        Just pass -> do
                          liftIO $ insertUser $ usr { userPassword = pass }
                          redirectToSavedRefererOrTo "/"
                          setCurrentUser (fromJust m_hmac_key) mdomain usr
                          flashSuccess "Account created."

   where userExists uN = findUser uN >>= return . isJust
         mkEdit usr = renderHtml . newUser . Just $ usr { userPassword = "" }
         wellformed s = s =~ ("^[a-zA-Z][a-zA-Z0-9_]*" :: String)
         blank s = s =~ ("^\\s*$" :: String)

  restEdit _ _ = do
    euser <- getCurrentUser (respondStat stat403)
    either id (\uName -> do muser <- liftIO $ findUser uName
                            maybe (respondStat stat403)
                                  (renderHtml .  editUser) muser) euser

  restUpdate _ _ = parseParams >> do
    euser <- getCurrentUser (respondStat stat403)
    either id (\uName -> do muser <- liftIO $ findUser uName
                            maybe (respondStat stat403) doUpdate muser) euser
      where blank s = s =~ ("^\\s*$" :: String)
            doUpdate user = paramOrBack "type" $ \type_ ->
              case type_ of
                "email"    -> changeEmail user
                "password" -> changePassword user
                _          -> respondStat stat403
            changeEmail user = paramOrBack "email" $ \email -> do
              unless (blank email) $
                liftIO $ updateUser $ user { userEmail = email }
              redirectBack
              if blank email 
                then flashError "Email cannot be blank."
                else flashSuccess "Changed email address."
            changePassword user = do
              paramOrBack "password_old"              $ \pass0 ->
                paramOrBack "password"                $ \pass1 ->
                  paramOrBack "password_confirmation" $ \pass2 -> do
                    case () of
                      _ | blank pass0 || blank pass1 -> 
                        redirectBack >> flashError "Password cannot be blank."
                      
                      _ | pass1 /= pass2 ->
                        redirectBack >> flashError "Passwords do not match."

                      _ | not $ validatePassword (S8.pack $ userPassword user)
                                                 (S8.pack pass0) ->
                        redirectBack >> flashError "Invalid password."
                      _ -> do mpass <- liftIO $ bcrypt pass1
                              case mpass of
                                Nothing -> respondStat stat500
                                Just pass -> do
                                  liftIO $ updateUser $ user { userPassword = pass }
                                  redirectBack
                                  flashSuccess "Password changed."



-- | Controller for login
-- POST /login
loginUser :: Action t L IO ()
loginUser = parseParams >> do
    env <- liftIO getEnvironment
    let m_hmac_key   = lookup "HMAC_KEY" env
        mdomain      = lookup "COOKIE_DOMAIN" env
    req <- getHttpReq
    paramOrBack "user_name"    $ \uName ->
        paramOrBack "password" $ \pass  -> do
          musr <- liftIO $ findUser uName
          let usr = fromJust musr -- takes advantage of Haskell's layzness
          case () of
            _ | isNothing musr -> mkLogin uName >> err
            _ | isNothing m_hmac_key -> respondStat stat500
            _ | not $ validatePassword (S8.pack $ userPassword usr)
                                       (S8.pack pass) -> mkLogin uName >> err
            _ -> do redirectToSavedRefererOrTo "/"
                    setCurrentUser (fromJust m_hmac_key) mdomain usr
                    flashSuccess "Account created."
  where err = flashError "Unknown username/password."
        mkLogin = renderHtml . loginView . Just

-- | GET /login
newLoginUser :: Action t L IO ()
newLoginUser = renderHtml $ loginView Nothing

-- | Controllerfor log out
logoutUser :: Action t L IO ()
logoutUser = do
  delCookie "_hails_user"
  delCookie "_hails_user_hmac"
  delCookie "_hails_referer"
  redirectTo "/login"
  flashSuccess "Logged out."

--
-- Helpers
--

-- | Hash password with bcrypt
bcrypt :: String -> IO (Maybe String)
bcrypt pass = do
  mres <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (S8.pack pass)
  return $ S8.unpack `liftM` mres


-- | Setthe hails user cookies
setCurrentUser :: String -> Maybe String -> User -> Action t b IO ()
setCurrentUser key mdomain usr = do
  let uName = userName usr
      domain = maybe "" (";domain="++) mdomain
      hmac  = showDigest $ hmacSha1 (L8.pack key) (L8.pack uName)
  setCookie "_hails_user" $ show uName ++ domain
  setCookie "_hails_user_hmac" $ show hmac ++ domain

-- | Get the username of the currently logged in user
--getCurrentUser :: HttpResp IO -> Either (HttpResp IO) String
getCurrentUser resp = do
  env <- liftIO getEnvironment
  case lookup "HMAC_KEY" env of
    Nothing -> return $ Left $ respondStat stat500
    Just key -> do
      req <- getHttpReq
      return $ maybe (Left resp) Right $ getAlreadyAuth req key

-- | Check cookies for existing user
getAlreadyAuth :: HttpReq t -> String -> Maybe String
getAlreadyAuth req key = 
  let cookies = reqCookies req
  in do user <- lookup "_hails_user" cookies
        mac0 <- lookup "_hails_user_hmac" cookies
        let mac1 = showDigest $ hmacSha1 (L8.pack key) (lazyfy user)
        if (S8.unpack mac0 == mac1)
          then return $ S8.unpack user
          else Nothing
    where lazyfy = L8.pack . S8.unpack


-- | Get parameter or redirect back
paramOrBack :: S -> (String -> Action t b IO ()) -> Action t b IO ()
paramOrBack n f = do
  mp <- param n
  maybe (redirectBack >> flashError "Incomplete form, try again.")
        (f . L8.unpack . paramValue) mp

