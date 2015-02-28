module Handler.Chat where

import Import hiding (atomically,(<>),ByteString)

import Yesod.WebSockets
import qualified Data.IntMap as IntMap
import Control.Concurrent.STM.Lifted
import Data.Aeson (encode,eitherDecode)
import Data.ByteString.Lazy.Internal

getChatR :: Int -> Handler Html
getChatR roomId = do
    (Entity _ user) <- requireAuth
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
        Just c -> webSockets $ chatApp roomId (userEmail user) c
    messages <- runDB $ selectList [MessageRoomId ==. roomId] [Asc MessageId]
    defaultLayout $ $(widgetFile "chat")

chatApp :: Int -> Text -> TChan ByteString -> WebSocketsT Handler ()
chatApp roomId username wChan = do
    time <- liftIO getCurrentTime
    let joinMsg = Message roomId username "event" " has joined the room" time
    let joinMsgDump = encode joinMsg
    _ <- lift . runDB $ insert joinMsg
    sendTextData joinMsgDump
    rChan <- atomically $ do
        writeTChan wChan joinMsgDump
        dupTChan wChan
    race_
        (forever $ atomically (readTChan rChan) >>= sendTextData)
        (sourceWS $$ mapM_C (handleMsg roomId username wChan))

handleMsg :: Int -> Text -> TChan ByteString -> ByteString -> WebSocketsT Handler ()
handleMsg roomId username wChan text = do
    case eitherDecode text of
        Left e -> do
            print e
            return ()
        Right baseMsg -> do
            time <- liftIO getCurrentTime
            let msg = baseMsg { messageRoomId = roomId
                              , messageUsername = username
                              , messageCreatedAt = time }
            print msg
            case messageType msg of
                "message" -> broadcastMsg wChan msg
                "event" -> broadcastMsg wChan msg
                _ -> return ()

broadcastMsg :: TChan ByteString -> Message -> WebSocketsT Handler ()
broadcastMsg wChan msg = do
    _ <- lift . runDB $ insert msg
    atomically $ do
        writeTChan wChan $ encode msg
