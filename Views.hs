{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views ( showView
             , loginView
             , newUser, editUser ) where

import Prelude hiding (div, span, id)
import Data.Monoid

import Models

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (label, form, span)

-- | Show main page: edit account, logout
showView :: String -> Html
showView user = 
  div ! class_ "hero-unit" $ do
    h1 $ toHtml $ "Welcome " ++ user ++ "!"
    p "It is a curious reason as to why you landed here."
    p $ do a ! href (toValue $ user ++ "/edit")
             ! class_ "btn btn-primary" $ "Edit account"
           " "
           a ! href "/logout" ! class_ "btn btn-inverse" $ "Log out"

-- | Show login form
loginView :: Maybe String -> Html
loginView muName = 
  form ! class_ "form-vertical" ! action "/login" ! method "POST" $ 
    fieldset $ do
      legend $ do 
        "Login"
        span ! class_ "create" $ do
          small "Don't have an account? "
          a ! class_ "btn btn-success" ! href "/users/new" $ "Register"
      label  "User Name"
      input ! type_ "text" ! placeholder "ron_swanson" ! name "user_name"
            ! maybe mempty (value . toValue) muName
      label  "Password"
      input ! type_ "password" ! name "password"
      div ! class_"form-actions" $ do
        input ! type_ "submit" ! class_ "btn btn-primary" ! value "Login"
        " "
        input ! type_ "reset" ! class_ "btn" ! value "Reset"

-- | Show account registration form
newUser :: Maybe User -> Html
newUser muser = 
  form ! class_ "form-vertical" ! action "/users" ! method "POST" $ 
    fieldset $ do
      legend "Create a new account"
      label  "User Name"
      input ! type_ "text" ! placeholder "ron" ! name "user_name"
            ! maybe mempty (value . toValue . userName) muser
      label  "Email"
      input ! type_ "email" ! placeholder "ron@swanson.me" ! name "email"
            ! maybe mempty (value . toValue . userEmail) muser
      label  "Password"
      input ! type_ "password" ! name "password"
      label  "Password confirmation"
      input ! type_ "password" ! name "password_confirmation"
      div ! class_"form-actions" $ do
        input ! type_ "submit" ! class_ "btn btn-primary" ! value "Register"
        " "
        input ! type_ "reset" ! class_ "btn" ! value "Reset"

-- | Show change email or password forms
editUser :: User -> Html
editUser user = do
  form ! class_ "form-vertical"
       ! action (toValue $ "/users/" ++ userName user) ! method "POST" $ 
    fieldset $ do
      legend "Update email"
      input ! type_ "hidden" ! name "type" ! value "email"
      label  "Email"
      input ! type_ "email" ! placeholder "ron@swanson.me" ! name "email"
            ! (value . toValue . userEmail $ user)
      div ! class_"form-actions" $ do
        input ! type_ "submit" ! class_ "btn btn-primary" ! value "Update email"
        " "
        input ! type_ "reset" ! class_ "btn" ! value "Reset"
  form ! class_ "form-vertical"
       ! action (toValue $ "/users/" ++ userName user) ! method "POST" $ 
    fieldset $ do
      legend "Change password"
      input ! type_ "hidden" ! name "type" ! value "password"
      label  "Old Password"
      input ! type_ "password" ! name "password_old"
      label  "New Password"
      input ! type_ "password" ! name "password"
      label  "Password confirmation"
      input ! type_ "password" ! name "password_confirmation"
      div ! class_"form-actions" $ do
        input ! type_ "submit" ! class_ "btn btn-primary" ! value "Change password"
        " "
        input ! type_ "reset" ! class_ "btn" ! value "Reset"
