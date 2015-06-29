module Helper.Auth
    ( requireAdminAuth
    , requireRoomAuth
    ) where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto ((?.))

requireAdminAuth :: Handler (Entity User)
requireAdminAuth = do
    userEntity <- requireAuth
    if isAdmin userEntity
        then return userEntity
        else redirect HomeR

requireRoomAuth :: RoomId -> Handler (Entity User)
requireRoomAuth roomId = do
    userEntity <- requireAuth
    accesses <- runDB $ accessesQuery roomId $ userId userEntity
    let hasAccess = isAdmin userEntity || length accesses > 1
    if hasAccess
        then return userEntity
        else redirect HomeR
  where
    userId (Entity uid _) = uid
    accessesQuery rid uid = do
        E.select $
            E.from $ \(room_access) -> do
            E.where_ $
                (room_access ?. RoomAccessRoomId E.==. E.just (E.val rid)) E.&&.
                (room_access ?. RoomAccessUserId E.==. E.just (E.val uid))
            return ( room_access )

isAdmin :: Entity User -> Bool
isAdmin (Entity _ user) = userIsAdmin user
