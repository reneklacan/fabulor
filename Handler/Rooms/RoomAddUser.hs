module Handler.Rooms.RoomAddUser where

import Import

getRoomAddUserR :: RoomId -> UserId -> Handler Html
getRoomAddUserR roomId userId = do
    _ <- runDB $ insert $ RoomAccess userId roomId (UTCTime (fromGregorian 0 0 0) 0)
    redirect $ RoomSettingsR roomId
