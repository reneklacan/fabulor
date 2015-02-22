module Handler.Chat where

import Import hiding (atomically,(<>))

import Yesod.WebSockets
import Data.Monoid ((<>))
import Control.Concurrent.STM.Lifted

chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    App { channel = wChan } <- getYesod
    rChan <- atomically $ do
        writeTChan wChan $ name <> " has joined the chat"
        dupTChan wChan
    race_
        (forever $ atomically (readTChan rChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan wChan $ name <> ": " <> msg))

getChatR :: Handler Html
getChatR = do
    webSockets chatApp
    defaultLayout $ $(widgetFile "chat")
