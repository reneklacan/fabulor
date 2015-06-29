module Handler.Rooms.RoomAddUser where

import Import

import Helper.Auth

getRoomAddUserR :: RoomId -> UserId -> Handler Html
getRoomAddUserR roomId userId = do
    _ <- requireAdminAuth
    _ <- runDB $ insert $ RoomAccess userId roomId Nothing
    redirect $ RoomSettingsR roomId
