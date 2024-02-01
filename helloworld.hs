{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module HelloWorld where

import Control.Applicative
import Data.Text (Text, unpack)
import Yesod
import Tautology (prettyPrintMap, processTextExpression, truthTable, prettyPrintTruthTable)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/input InputR GET
|]

instance Yesod App where 
    approot = ApprootStatic "https://mattjam.me" -- Update to use HTTPS

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data PropInput = PropInput
    { str :: Text
    }
    deriving Show

base :: Widget
base = [whamlet|
<form action=@{InputR}>
    <h3>Instructions 
    <p>Input a valid boolean expression using the following key
    <table>
        <tr>
            <td>True
            <td>T
        <tr>
            <td>False
            <td>F
        <tr>
            <td>Variable 
            <td>Single character
        <tr>
            <td>And
            <td>*
        <tr>
            <td>Or
            <td>+
        <tr>
            <td>Not
            <td>-
    <p>Example: (a+b)*(-c)
    <h3>Input:
    <p>
        <input type=text name=str>
        <input type=submit value="Process">
|]

getHomeR :: Handler Html
getHomeR = defaultLayout base

getInputR :: Handler Html
getInputR = do
    prop <- runInputGet $ PropInput
                <$> ireq textField "str"
    defaultLayout $ do
        base 
        let p = processTextExpression (unpack $ str prop)
        [whamlet|
            Simplified expression:
            <p>#{show p }
            <h3>Truth table: 
            <pre>#{preEscapedToMarkup (prettyPrintTruthTable p)}
        |]

main :: IO ()
main = do
    let tlsSettings' = tlsSettings "/etc/nginx/ssl/mattjam_me.crt" "/etc/nginx/ssl/mattjam.me.key"
    runTLS tlsSettings' $ warp 3000 App 
