module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance FromJSON Message where
    parseJSON (Object v) =
        Message
        <$> v .: "roomId"
        <*> v .: "username"
        <*> v .: "type"
        <*> v .: "value"
        <*> v .: "createdAt"
    parseJSON _ = mzero

instance ToJSON Message where
    toJSON (Message roomId username mtype value createdAt)
        = object
            [ "username" .= username
            , "type" .= mtype
            , "value" .= value
            , "roomId" .= roomId
            , "createdAt" .= createdAt
            ]
