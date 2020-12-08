{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Implantation where

import Import

formImplantation :: Form (Implantation, Text)
formImplantation = renderBootstrap $ (,)
    <$> (Implantation
        <$> areq textField "Frequência: " Nothing
        <*> areq textField "Observação: " Nothing)
    <*>

getImplantationR :: Maybe PatientId -> Handler Html
getImplantationR pid = do
    (widget,_) <- generateFormPost formImplantation
    defaultLayout [whamlet|
        <form action=@{ImplantationR pid} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar">
    |]

postImplantationR :: PatientId -> Handler Html
postImplantationR pid = do
    Just (Entity patientId _) -> do
        _ <- runDB $ insertEntity (Implantation patientId)
        redirect (PDescR pid)

postIDeleteR :: ImplantationId -> Handler Html
postIDeleteR iid = do
    runDB $ delete iid
    redirect PListR