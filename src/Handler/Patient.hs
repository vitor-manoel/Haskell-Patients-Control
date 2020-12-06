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
<<<<<<< HEAD
    <*> areq intField "Idade: " Nothing
    <*> areq textField "Endereço: " Nothing
    <*> areq textField "Cidade: " Nothing
    <*> areq textField "Estado: " Nothing
    <*> areq textField "Sexo: " Nothing
    <*> areq textField "Telefone: " Nothing

=======
    <$> areq intField "Idade: " Nothing
    <$> areq textField "Endereço: " Nothing
    <$> areq textField "Cidade: " Nothing
    <$> areq textField "Estado: " Nothing
    <$> areq textField "Sexo: " Nothing
    <$> areq textField "Telefone: " Nothing
    
>>>>>>> 6aa31fdd24e910abe68820f1f914b8e79e38af73
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
            redirect ()
        _ -> redirect HomeR