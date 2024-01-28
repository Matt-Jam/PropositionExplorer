{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module HelloWorld where
import           Control.Applicative
import           Data.Text           (Text, unpack)
import           Yesod
import Tautology (prettyPrintMap, processTextExpression, truthTable, prettyPrintTruthTable)



data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/input InputR GET
|]

instance Yesod App where 
    approot _ = ApprootStatic "http://mattjam.me"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data PropInput = PropInput
    { str :: Text
    }
    deriving Show

getHomeR :: Handler Html
getHomeR = defaultLayout 
    [whamlet|
            <form action=@{InputR}>
                <h3>
                    Input:
                <p>
                    <input type=text name=str>
                    <input type=submit value="Process">
    |]

getInputR :: Handler Html
getInputR = do
    prop <- runInputGet $ PropInput
                <$> ireq textField "str"
    defaultLayout $ do
        let p = processTextExpression (unpack $ str prop)
        [whamlet|
            <form action=@{InputR}>
                <h3>
                    Input:
                <p>
                    <input type=text name=str>
                    <input type=submit value="Process">
                <h3>
                    Simplified expression:
                <p>
                    #{show p }
                <h3>
                    Truth table: 
                <pre>
                    #{preEscapedToMarkup (prettyPrintTruthTable p)}
        |]
        

main :: IO ()
main = warp 3000 App