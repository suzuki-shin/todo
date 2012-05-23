{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getTagCreateR :: Handler RepHtml
getTagCreateR = do
    (formWidget, formEnctype) <- generateFormPost tagForm
    defaultLayout $ do
        setTitle "TagCreate"
        $(widgetFile "tagform")

postTagCreateR :: Handler RepHtml
postTagCreateR = do
    ((result, formWidget), formEnctype) <- runFormPost tagForm
    defaultLayout $ do
        setTitle "TagCreate"
        $(widgetFile "tagform")

tagForm :: Form Tag
tagForm = renderDivs $ Tag
    <$> areq textField "Name" Nothing

getUserCreateR :: Handler RepHtml
getUserCreateR = do
    (formWidget, formEnctype) <- generateFormPost userForm
    defaultLayout $ do
        setTitle "UserCreate"
        $(widgetFile "userform")

postUserCreateR :: Handler RepHtml
postUserCreateR = do
    ((result, formWidget), formEnctype) <- runFormPost userForm
    defaultLayout $ do
        setTitle "UserCreate"
        $(widgetFile "userform")

userForm :: Form User
userForm = renderDivs $ User
    <$> areq textField "IDENT" Nothing
    <*> aopt textField "PASSWORD" Nothing

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
