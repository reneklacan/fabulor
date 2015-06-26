module Handler.Rooms.RoomRemoveUser where

import Import

getRoomRemoveUserR :: RoomId -> UserId -> Handler Html
getRoomRemoveUserR roomId userId = do
    runDB $ deleteWhere [ RoomAccessRoomId ==. roomId, RoomAccessUserId ==. userId]
    redirect $ RoomSettingsR roomId
