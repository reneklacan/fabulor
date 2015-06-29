module Handler.Rooms.RoomAddUser where

import Import

import Helper.Auth

getRoomAddUserR :: RoomId -> UserId -> Handler Html
getRoomAddUserR roomId userId = do
    _ <- requireAdminAuth
    currentTime <- liftIO getCurrentTime
    _ <- runDB $ insert $ RoomAccess userId roomId currentTime
    redirect $ RoomSettingsR roomId
