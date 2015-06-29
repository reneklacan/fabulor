module Handler.Rooms.RoomDelete where

import Import

import Helper.Auth

getRoomDeleteR :: RoomId -> Handler Html
getRoomDeleteR roomId = do
    _ <- requireAdminAuth
    runDB $ deleteWhere [RoomId ==. roomId]
    redirect HomeR
