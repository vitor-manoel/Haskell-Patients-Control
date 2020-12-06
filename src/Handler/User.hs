{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.User where

import Import

formUser :: Form (User, Text)
formUser = renderBootstrap $ (,)
    <$> (User
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Digite Novamente: " Nothing


getRegisterR :: Handler Html
getRegisterR = do
    (widget,_) <- generateFormPost formUser
    msg <- getMessage
    defaultLayout $ do
        $(widgetFile "user")


postRegisterR :: Handler Html
postRegisterR = do
    ((result,_),_) <- runFormPost formUser
    case result of
        FormSuccess (user,verifica) -> do
            user <- runDB $ getBy (UniqueUser (userEmail user))
            case user of
                Nothing -> do
                    if (userPassword user == verifica) then do
                        _ <- runDB $ insert user
                        setMessage [shamlet|
                            <div .alert.alert-success role="alert">
                                Cadastrado com sucesso.
                        |]
                        redirect ULoginR
                    else do
                        setMessage [shamlet|
                            <div .alert.alert-danger role="alert">
                                Falha ao realizar cadastro.
                        |]
                        redirect URegisterR
                Just(Entity _ _) -> do
                    setMessage [shamlet|
                        <div .alert.alert-danger role="alert">
                            Este e-mail já está em uso !
                    |]
                    redirect URegisterR
        _ -> redirect HomeR