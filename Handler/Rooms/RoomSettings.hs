module Handler.Rooms.RoomSettings where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.),(?.))
import Database.Persist.Sql (fromSqlKey)

import Helper.Auth

data AccessInfo = AccessInfo
    { accessInfoUserId :: UserId
    , accessInfoUser :: User
    , accessInfoHasAccess :: Bool
    } deriving Show

getRoomSettingsR :: RoomId -> Handler Html
getRoomSettingsR roomId = do
    _ <- requireAdminAuth
    usersWithAccess' <- runDB $ do
        E.select $
            E.from $ \(user `E.LeftOuterJoin` room_access) -> do
            E.on $ E.just (user ^. UserId) E.==. room_access ?. RoomAccessUserId
            E.where_ $ room_access ?. RoomAccessRoomId E.==. E.just (E.val roomId)
            return ( user ^. UserId )
    users' <- runDB $ do
        selectList [] [Asc UserId]

    let usersWithAccess = fmap E.unValue usersWithAccess'
    let mapF = (\(Entity uid user) -> AccessInfo uid user $ oelem uid usersWithAccess)
    let users = fmap mapF users'

    defaultLayout $ $(widgetFile "room-settings")
