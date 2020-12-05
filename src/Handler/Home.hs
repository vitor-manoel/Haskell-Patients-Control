{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
            <h1>
                SISTEMA DE PRODUTOS
        
            <img src=@{StaticR imgs_produto_jpg}>
        
            <ul>
                <li>
                    <a href=@{ProdutoR}>
                        CADASTRO DE PRODUTOS
                        
                <li>
                    <a href=@{ListaR}>
                        LISTAGEM
        |]
