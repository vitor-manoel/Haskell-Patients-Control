{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.User where

import Import

formUser :: Form (User, Text)
formUser mp = renderDivs $ User
    <$> 
        <$> areq textField "Nome: " (fmap userName mp)
        <*> areq emailField "E-mail: " (fmap userEmail mp)
        <*> areq passwordField "Senha: " (fmap userPassword mp)
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
        FormSuccess (xuser,verifica) -> do
            xuser <- runDB $ getBy (UniqueEmail (userEmail xuser))
            case xuser of
                Nothing -> do
                    if (userPassword xuser == verifica) then do
                        _ <- runDB $ insert xuser
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
        _ -> redirect ULoginR