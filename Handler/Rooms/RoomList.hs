module Handler.Rooms.RoomList where

import Import

--import Database.Persist.Sql (fromSqlKey)

getRoomListR :: Handler Html
getRoomListR = do
    (Entity _ user) <- requireAuth
    rooms <- runDB $ selectList [] [Desc RoomId]
    defaultLayout $ do
        $(widgetFile "room-list")
