{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Patient where

import Import

formPatient :: Maybe Patient -> Form Patient
formPatient mp = renderDivs $ Patient
    <$> areq textField "Nome: " (fmap patientName mp)
    <*> areq intField "Idade: " (fmap patientAge mp)
    <*> areq textField "Endereço: " (fmap patientAddress mp)
    <*> areq textField "Cidade: " (fmap patientCity mp)
    <*> areq textField "Estado: " (fmap patientState mp)
    <*> areq textField "Sexo: " (fmap patientGender mp)
    <*> areq textField "Telefone: " (fmap patientPhone mp)

auxPatientR :: Route App -> Maybe Patient -> Handler Html
auxPatientR rt mp = do
    (widget,_) <- generateFormPost (formPatient mp)
    defaultLayout [whamlet|
        <form action=@{rt} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar">
    |]

getPatientR :: Handler Html
getPatientR = auxPatientR PatientR Nothing
    
postPatientR :: Handler Html
postPatientR = do
    ((res,_),_) <- runFormPost (formPatient Nothing)
    case res of
        FormSuccess patient -> do
            pid <- runDB (insert patient)
            redirect (PDescR pid)
        _ -> redirect PListR

getPDescR :: PatientId -> Handler Html
getPDescR pid = do
    patient <- runDB $ get404 pid
    defaultLayout [whamlet|
        <h1>
            Nome : #{patientName patient}

        <h2>
            Idade : #{patientAge patient}    
    |]

getPListR :: Handler Html
getPListR = do
    patients <- runDB $ selectList [] []
    defaultLayout [whamlet|
        <table>
            <thead>
                <tr>
                    <th>
                        Nome
                    <th>
                        Idade
                    <th>

                    <th>

                    <th>
            
            <tbody>
                $forall Entity pid patient <- patients
                    <tr>
                        <td>
                            #{patientName patient}

                        <td>
                            #{patientAge patient}

                        <td>
                            <a href=@{PDescR pid}>
                                Visualizar

                        <td>
                            <a href=@{PChangeR pid}>
                                Editar

                        <td>
                            <form action=@{PDeleteR pid} method=post>
                                <input type="submit" value="X">
        <form to={"/patient"}> 
            <input type="submit" value="Cadastrar">
                        
    |]

getPChangeR :: PatientId -> Handler Html
getPChangeR pid = do
    antigo <- runDB $ get404 pid
    auxPatientR (PChangeR pid) (Just antigo)

postPChangeR :: PatientId -> Handler Html
postPChangeR pid = do
    ((res,_),_) <- runFormPost (formPatient Nothing)
    case res of
        FormSuccess novoP -> do
            _ <- runDB (replace pid novoP)
            redirect PListR
        _ -> redirect HomeR

postPDeleteR :: PatientId -> Handler Html
postPDeleteR pid = do
    runDB $ delete pid
    redirect PListR