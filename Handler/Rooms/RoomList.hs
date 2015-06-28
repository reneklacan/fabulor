module Handler.Rooms.RoomList where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.),(?.))

getRoomListR :: Handler Html
getRoomListR = do
    (Entity userId user) <- requireAuth
    rooms <- runDB $ roomsQuery userId $ userIsAdmin user
    defaultLayout $ do
        $(widgetFile "room-list")
  where
    roomsQuery _ True = do
        selectList [] [Desc RoomId]
    roomsQuery userId False = do
        E.select $
            E.from $ \(room `E.LeftOuterJoin` room_access) -> do
            E.on $ E.just (room ^. RoomId) E.==. room_access ?. RoomAccessRoomId
            E.where_ $ room_access ?. RoomAccessUserId E.==. E.just (E.val userId)
            return ( room )
