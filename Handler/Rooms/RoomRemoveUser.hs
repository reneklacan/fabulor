module Handler.Rooms.RoomRemoveUser where

import Import

import Helper.Auth

getRoomRemoveUserR :: RoomId -> UserId -> Handler Html
getRoomRemoveUserR roomId userId = do
    _ <- requireAdminAuth
    runDB $ deleteWhere [ RoomAccessRoomId ==. roomId, RoomAccessUserId ==. userId]
    redirect $ RoomSettingsR roomId
