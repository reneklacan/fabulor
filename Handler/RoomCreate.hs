module Handler.RoomCreate where

import Import

import Helper.Bootstrap
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getRoomCreateR :: Handler Html
getRoomCreateR = do
    (roomWidget, enctype) <- generateFormPost roomForm
    defaultLayout $ do
        $(widgetFile "room-create")

postRoomCreateR :: Handler Html
postRoomCreateR = do
    ((res, roomWidget), enctype) <- runFormPost roomForm
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

