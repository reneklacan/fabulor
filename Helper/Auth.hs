module Helper.Auth where

import Import

requireAdminAuth :: Handler (Entity User)
requireAdminAuth = do
    userEntity <- requireAuth
    if isAdmin userEntity
        then return userEntity
        else redirect HomeR
  where
    isAdmin (Entity _ user) = userIsAdmin user
