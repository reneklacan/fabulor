module Handler.Chat where

import Import hiding (atomically,(<>),ByteString)

import Yesod.WebSockets
import Control.Concurrent.STM.Lifted
import Data.Aeson (encode,eitherDecode)
import Data.ByteString.Lazy.Internal
import qualified Data.Map as M

data Chat = Chat { roomId :: RoomId
                 , room :: Room
                 , userId :: UserId
                 , user :: User
                 , channel :: TChan ByteString }

getChatR :: RoomId -> Handler Html
getChatR roomId = do
    (Entity userId user) <- requireAuth
    room <- runDB $ get404 roomId
    App {..} <- getYesod
    chan <- liftIO $ atomically $ do
        m <- readTVar channels
        case M.lookup roomId m of
            Nothing -> do
                c <- newBroadcastTChan
                writeTVar channels $ M.insert roomId c m
                return $ Just c
            Just c -> fmap Just $ dupTChan c
    case chan of
        Nothing -> return ()
        Just c -> webSockets $ chatApp $ Chat roomId room userId user c
    messages <- runDB $ selectList [MessageRoomId ==. roomId] [Asc MessageId]
    defaultLayout $ $(widgetFile "chat")

chatApp :: Chat -> WebSocketsT Handler ()
chatApp chat = do
    time <- liftIO getCurrentTime
    let joinMsg = Message (roomId chat) (userEmail $ user chat) "event" " has joined the room" time
    let joinMsgDump = encode joinMsg
    _ <- lift . runDB $ insert joinMsg
    sendTextData joinMsgDump
    rChan <- atomically $ do
        writeTChan (channel chat) joinMsgDump
        dupTChan (channel chat)
    race_
        (forever $ atomically (readTChan rChan) >>= sendTextData)
        (sourceWS $$ mapM_C (handleMsg chat))

handleMsg :: Chat -> ByteString -> WebSocketsT Handler ()
handleMsg chat text = do
    case eitherDecode text of
        Left e -> do
            print e
            return ()
        Right msg -> do
            print msg
            case messageType msg of
                "message" -> broadcastMsg chat msg
                "event" -> broadcastMsg chat msg
                "ping" -> checkMsg chat
                _ -> return ()

broadcastMsg :: Chat -> Message -> WebSocketsT Handler ()
broadcastMsg chat baseMsg = do
    time <- liftIO getCurrentTime
    let msg = baseMsg { messageRoomId = roomId chat
                      , messageUsername = userEmail $ user chat
                      , messageCreatedAt = time }
    _ <- lift . runDB $ insert msg
    atomically $ do
        writeTChan (channel chat) $ encode msg

checkMsg :: Chat -> WebSocketsT Handler ()
checkMsg chat = do
    time <- liftIO getCurrentTime
    _ <- lift . runDB $ do
        updateWhere [RoomAccessUserId ==. userId chat, RoomAccessRoomId ==. roomId chat]
                    [RoomAccessLastSeenAt =. time]
    putStrLn "ping"
