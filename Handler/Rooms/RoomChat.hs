module Handler.Rooms.RoomChat where

import Import hiding (atomically,(<>),ByteString)

import Yesod.WebSockets
import Control.Concurrent.STM.Lifted
import Data.Aeson (encode,eitherDecode)
import Data.ByteString.Lazy.Internal
import qualified Data.Map as M
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

data Chat = Chat
    { chatRoomId :: RoomId
    , chatRoom :: Room
    , chatUserId :: UserId
    , chatUser :: User
    , chatChannel :: TChan ByteString
    }

data RoomUserStatus = RoomUserStatus
    { statusUserId :: UserId
    , statusUserEmail :: Text
    , statusUserLastSeenAt :: UTCTime
    } deriving Show

instance ToJSON RoomUserStatus where
    toJSON (RoomUserStatus userId email lastSeenAt)
        = object
            [ "userId" .= userId
            , "username" .= email
            , "lastSeenAt" .= lastSeenAt ]

getRoomChatR :: RoomId -> Handler Html
getRoomChatR roomId = do
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
    defaultLayout $ $(widgetFile "room-chat")

chatApp :: Chat -> WebSocketsT Handler ()
chatApp chat = do
    time <- liftIO getCurrentTime
    let joinMsg = Message (chatRoomId chat) (userEmail $ chatUser chat) "event" " has joined the room" time
    let joinMsgDump = encode joinMsg
    _ <- lift . runDB $ insert joinMsg
    sendTextData joinMsgDump
    rChan <- atomically $ do
        writeTChan (chatChannel chat) joinMsgDump
        dupTChan $ chatChannel chat
    race_
        (forever $ atomically (readTChan rChan) >>= sendTextData)
        (sourceWS $$ mapM_C $ handleMsg chat)

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
                "status" -> statusMsg chat
                _ -> return ()

broadcastMsg :: Chat -> Message -> WebSocketsT Handler ()
broadcastMsg chat baseMsg = do
    time <- liftIO getCurrentTime
    let msg = baseMsg { messageRoomId = chatRoomId chat
                      , messageUsername = userEmail $ chatUser chat
                      , messageCreatedAt = time }
    _ <- lift . runDB $ insert msg
    atomically $ do
        writeTChan (chatChannel chat) $ encode msg

statusMsg :: Chat -> WebSocketsT Handler ()
statusMsg chat = do
    time <- liftIO getCurrentTime
    statusEntities <- lift . runDB $ do
        updateWhere
            [RoomAccessUserId ==. chatUserId chat, RoomAccessRoomId ==. chatRoomId chat]
            [RoomAccessLastSeenAt =. time]
        E.select $
            E.from $ \(user `E.InnerJoin` room_access) -> do
            E.on $ user ^. UserId E.==. room_access ^. RoomAccessUserId
            E.where_ $ room_access ^. RoomAccessRoomId E.==. (E.val (chatRoomId chat))
            return ( user ^. UserId
                   , user ^. UserEmail
                   , room_access ^. RoomAccessLastSeenAt )
    atomically $ do
        writeTChan (chatChannel chat) $ encode $ fmap userStatusFromEntity statusEntities

userStatusFromEntity :: (E.Value UserId, E.Value Text, E.Value UTCTime) -> RoomUserStatus
userStatusFromEntity (userId, email, lastSeenAt) =
    RoomUserStatus (E.unValue userId) (E.unValue email) (E.unValue lastSeenAt)
