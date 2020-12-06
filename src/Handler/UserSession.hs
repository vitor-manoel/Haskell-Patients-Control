{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import

formLogin :: Form (Text, Text)
formLogin = renderBootstrap $ (,)
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing


getULoginR :: Handler Html
getULoginR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        $(widgetFile "entrar")


postULoginR :: Handler Html
postULoginR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (email,password) -> do
           user <- runDB $ getBy (UniqueEmail email)
           case user of
                Nothing -> do
                    setMessage [shamlet|
                        <div .alert.alert-danger role="alert">
                            E-mail não encontrado.
                    |]
                    redirect ULoginR
                Just (Entity _ user) -> do
                    if (userPassword user == password) then do
                        setSession "_EMAIL" (userEmail user)
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            <div .alert.alert-danger role="alert">
                                Senha incorreta.
                        |]
                        redirect EntrarR
        _ -> redirect HomeR

getULogoutR :: Handler Html
getULogtouR = do
    deleteSession "_EMAIL"
    redirect HomeR