module Handler.RoomCreate where

import Import

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

bootstrapField :: Text -> (Text -> a) -> Text -> Field Handler a
bootstrapField tag fromText label = Field
    { fieldParse = \rawVals _ ->
        case rawVals of
            [a] -> return $ Right $ Just $ fromText a
            _ -> return $ Right Nothing
    , fieldView = \tagId attrName attrs _ _ ->
        [whamlet|
            <div.form-group>
                <label.control-label.col-sm-2 for=#{tagId}>#{label}
                <div.col-sm-10>
                    <input.form-control id=#{tagId} name=#{attrName} *{attrs} type=#{tag}>
        |]
     , fieldEnctype = UrlEncoded
     }

bootstrapTextField :: Text -> Field Handler Text
bootstrapTextField = bootstrapField "text" id
