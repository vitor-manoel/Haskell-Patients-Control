{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Implantation where

import Import

formImplantation :: Maybe Implantation -> Form Implantation
formImplantation mp = renderDivs $ Implantation
    <$> areq textField "Frequência: " (fmap ImplantationFraquency mp)
    <*> areq textField "Observação: " (fmap ImplantationObservation mp)

getImplantationR :: ImplantationId -> Handler Html
getImplantationR iid = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        ^{widget}                    

postIDeleteR :: ImplantationId -> Handler Html
postIDeleteR iid = do
    runDB $ delete iid
    redirect PListR