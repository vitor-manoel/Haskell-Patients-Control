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
    <$> areq textField (FieldSettings "Nome: "
                        (Just "Nome do Paciente")
                        (Just "hs1")
                        Nothing
                        [("class", "classe1")]
                        ) Nothing
    <$> areq textField "...: " Nothing

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
        FormSucess patient -> do
            pid <- runDB (insert patient)
            redirect ()
        _ -> redirect HomeR