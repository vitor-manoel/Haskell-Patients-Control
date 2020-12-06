{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Patient where

import Import

formPatient :: Form Patient
formPatient = renderDivs $ Patient
    <$> areq textField "Nome: " Nothing
    <*> areq intField "Idade: " Nothing
    <*> areq textField "Endereço: " Nothing
    <*> areq textField "Cidade: " Nothing
    <*> areq textField "Estado: " Nothing
    <*> areq textField "Sexo: " Nothing
    <*> areq textField "Telefone: " Nothing

getPatientR :: Handler Html
getPatientR = do
    (widget,_) <- generateFormPost formPatient
    defaultLayout [whamlet|
        <form action=@{PatientR} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar">
    |]
    
postPatientR :: Handler Html
postPatientR = do
    ((res,_),_) <- runFormPost formPatient
    case res of
        FormSuccess patient -> do
            pid <- runDB (insert patient)
            redirect (DescR pid)
        _ -> redirect HomeR

getDescR :: PatientId -> Handler Html
getDescR pid = do
    patient <- runDB $ get404 pid
    defaultLayout [whamlet|
        <h1>
            Nome : #{patientName patient}

        <h2>
            Idade : #{patientAge patient}    
    |]