module Handler.Chat where

import Import hiding (atomically,(<>))

import Yesod.WebSockets
import qualified Data.IntMap as IntMap
import Data.Monoid ((<>))
import Control.Concurrent.STM.Lifted

chatApp :: TChan Text -> WebSocketsT Handler ()
chatApp wChan = do
  sendTextData ("Welcome to the chat server, please enter your name." :: Text)
  name <- receiveData
  sendTextData $ "Welcome, " <> name
  rChan <- atomically $ do
    writeTChan wChan $ name <> " has joined the chat"
    dupTChan wChan
  race_
    (forever $ do
      atomically (readTChan rChan) >>= sendTextData
    )
    (sourceWS $$ mapM_C (\msg -> do
      _ <- lift . runDB $ insert $ Message msg 2 name
      atomically $ do
        writeTChan wChan $ name <> ": " <> msg
    ))

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
    Just c -> webSockets $ chatApp c
  defaultLayout $ $(widgetFile "chat")
