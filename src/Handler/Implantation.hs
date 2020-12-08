{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Implantation where

import Import

formImp :: Form (Implantation, Text)
formImp = renderBootstrap $ (,)
    <$> (Implantation
        <$> areq textField "Frequência: " Nothing
        <*> areq textField "Observação: " Nothing
    <*> 

postIDeleteR :: ImplantationId -> Handler Html
postIDeleteR impId = do
    runDB $ delete impId
    redirect PListR

postImplantationR :: ImplantationId -> Handler Html
postImplantationR implantationId = do
    ((res,_),_) <- runFormPost (formImplantation Nothing)
    case res of
        FormSuccess implantation -> do
            pid <- runDB (insert implantation)
            redirect PListR
        _ -> redirect PListR
    
