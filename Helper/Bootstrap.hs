module Helper.Bootstrap where

import Import

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), BootstrapGridOptions(..))

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

bootstrapHorizontalForm :: BootstrapFormLayout
bootstrapHorizontalForm = BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10)
