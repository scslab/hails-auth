{-# LANGUAGE OverloadedStrings #-}

module Layouts where

import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Http.Support

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title, span, content)
import qualified Text.Blaze.Renderer.Utf8 as R (renderHtml)

import Control.Monad.Trans
import System.Environment

renderHtml :: Html -> Action t b IO ()
renderHtml htmlBody = do
  env <- liftIO getEnvironment
  let brand = fromMaybe "Hails Authentication" $ lookup "BRAND" env
      brandUrl = fromMaybe "#" $ lookup "BRAND_URL" env
  render "text/html" $ R.renderHtml $ application brand brandUrl htmlBody

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

application :: String -> String -> Html -> Html
application brand brandUrl content = docTypeHtml $ do
  head $ do
    title $ "GitStar"
    stylesheet "/css/bootstrap.css"
    stylesheet "/css/application.css"
    body $ do
     div ! class_ "navbar navbar-fixed-top" $ do
       div ! class_ "navbar-inner" $ do
         div ! class_ "container" $ do
           a ! href (toValue brandUrl) ! class_ "brand" $ toHtml brand
     div ! class_ "row" $
       div ! id "flash-messages" ! class_ "span4 offset4" $ ""
     div ! class_ "container" $ content
     script ! src "/js/jquery.js" $ ""
     script ! src "/js/jquery.cookie.js" $ ""
     script ! src "/js/bootstrap.min.js" $ ""
     script ! src "/js/bootstrap-typeahead.js" $ ""
     script ! src "/js/flash.js" $ ""
