{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Tag where

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
