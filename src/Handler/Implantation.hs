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

getImplantationR :: PatientId -> Handler Html
getImplantationR iid = do
    (widget,_) <- generateFormPost formImplantation
    defaultLayout [whamlet|
        <form action=@{rt} method=post>
            ^{widget}
            {<*> areq intField "Paciente: " iid}
            <input type="submit" value="Cadastrar">
    |]

postIDeleteR :: ImplantationId -> Handler Html
postIDeleteR iid = do
    runDB $ delete iid
    redirect PListR