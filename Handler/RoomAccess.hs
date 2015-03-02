module Handler.RoomAccess where

import Import

postRoomAccessR :: RoomId -> UserId -> Handler Html
postRoomAccessR roomId userId = do
    --time <- liftIO getCurrentTime
    _ <- runDB $ insert $ RoomAccess userId roomId (UTCTime (fromGregorian 0 0 0) 0)
    return ""

deleteRoomAccessR :: RoomId -> UserId -> Handler Html
deleteRoomAccessR roomId userId = do
    runDB $ deleteWhere [ RoomAccessRoomId ==. roomId, RoomAccessUserId ==. userId]
    return ""
