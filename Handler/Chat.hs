module Handler.Chat where

import Import hiding (atomically,(<>))

import Yesod.WebSockets
import qualified Data.IntMap as IntMap
import Data.Monoid ((<>))
import Control.Concurrent.STM.Lifted

chatApp :: Int -> TChan Text -> WebSocketsT Handler ()
chatApp roomId wChan = do
    name <- receiveData
    let joinMsg = Message roomId name " has joined the room"
    let joinMsgDump = dumpMessage joinMsg
    _ <- lift . runDB $ insert joinMsg
    sendTextData joinMsgDump
    rChan <- atomically $ do
        writeTChan wChan joinMsgDump
        dupTChan wChan
    race_
        (forever $ do
            atomically (readTChan rChan) >>= sendTextData
        )
        (sourceWS $$ mapM_C (\text -> do
            let msg = Message roomId name text
            _ <- lift . runDB $ insert msg
            atomically $ do
                writeTChan wChan $ dumpMessage msg
        ))

-- TODO: Use Aeson intead of this logic
dumpMessage :: Message -> Text
dumpMessage message = do
    "{" <> \
        "\"username\":\"" <> (messageUsername message) <> "\"," <> \
        "\"text\":\"" <> (messageText message) <> "\"" <> \
    "}"

getChatR :: Int -> Handler Html
getChatR roomId = do
    App {..} <- getYesod
    chan <- liftIO $ atomically $ do
        m <- readTVar channels
        case IntMap.lookup roomId m of
            Nothing -> do
                c <- newBroadcastTChan
                writeTVar channels $ IntMap.insert roomId c m
                return $ Just c
            Just c -> fmap Just $ dupTChan c
    case chan of
        Nothing -> return ()
        Just c -> webSockets $ chatApp roomId c
    messages <- runDB $ selectList [MessageRoomId ==. roomId] [Asc MessageId]
    defaultLayout $ $(widgetFile "chat")
