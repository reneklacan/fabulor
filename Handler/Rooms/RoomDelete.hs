module Handler.Rooms.RoomDelete where

import Import

getRoomDeleteR :: RoomId -> Handler Html
getRoomDeleteR roomId = do
    runDB $ deleteWhere [RoomId ==. roomId]
    redirect HomeR
