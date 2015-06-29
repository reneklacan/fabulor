module Handler.Rooms.RoomCreate where

import Import

import Yesod.Form.Bootstrap3 (renderBootstrap3)

import Helper.Auth
import Helper.Bootstrap

getRoomCreateR :: Handler Html
getRoomCreateR = do
    _ <- requireAdminAuth
    (roomWidget, enctype) <- generateFormPost roomForm
    defaultLayout $ do
        $(widgetFile "room-create")

postRoomCreateR :: Handler Html
postRoomCreateR = do
    _ <- requireAdminAuth
    ((res, _), _) <- runFormPost roomForm
    case res of
        FormSuccess room -> do
            _ <- runDB $ insert room
            setMessage $ toHtml $ (roomName room) <> " created"
            redirect $ HomeR
        _ -> defaultLayout $ do
            setMessage "Creation failed"
            redirect $ HomeR

roomForm :: Form Room
roomForm = renderDivs $ Room
    <$> areq (bootstrapTextField "Name") "" Nothing

-- form-control class is missing in generated input
roomForm' :: Form Room
roomForm' = renderBootstrap3 bootstrapHorizontalForm $ Room
    <$> areq textField "Name" Nothing
