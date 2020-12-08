{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import

formLogin :: Form (Text, Text)
formLogin = renderBootstrap $ (,)
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing


getHomeR :: Handler Html
getHomeR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        $(widgetFile "userSession")


postHomeR :: Handler Html
postHomeR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (email,senha) -> do
           usuario <- runDB $ getBy (UniqueEmail email)
           case usuario of
                Nothing -> do
                    setMessage [shamlet|
                        <div .alert.alert-danger role="alert">
                            E-mail não se encontra na base de dados.
                    |]
                    redirect HomeR
                Just (Entity _ usu) -> do
                    if (userPassword usu == senha) then do
                        setSession "_EMAIL" (userEmail usu)
                        redirect PListR
                    else do
                        setMessage [shamlet|
                            <div .alert.alert-danger role="alert">
                                Senha incorreta.
                        |]
                        redirect HomeR
        _ -> redirect HomeR

getSairR :: Handler Html
getSairR = do
    deleteSession "_EMAIL"
    redirect HomeR

