module Handler.Chat where

import Import hiding (atomically,(<>),ByteString)

import Yesod.WebSockets
import qualified Data.IntMap as IntMap
import Control.Concurrent.STM.Lifted
import Data.Aeson (encode)
import Data.ByteString.Lazy.Internal

chatApp :: Int -> Text -> TChan ByteString -> WebSocketsT Handler ()
chatApp roomId username wChan = do
    jTime <- liftIO getCurrentTime
    let joinMsg = Message roomId username "event" " has joined the room" jTime
    let joinMsgDump = encode joinMsg
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
            time <- liftIO getCurrentTime
            let msg = Message roomId username "message" text time
            _ <- lift . runDB $ insert msg
            atomically $ do
                writeTChan wChan $ encode msg
        ))

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
