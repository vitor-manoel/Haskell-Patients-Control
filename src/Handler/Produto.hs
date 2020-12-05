{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Produto where

import Import

-- Gera o html das caixas de texto do form
formProduto :: Maybe Produto -> Form Produto
formProduto mp = renderDivs $ Produto 
    <$> areq textField (FieldSettings "Nome: "
                                      Nothing
                                      (Just "hs1")
                                      Nothing
                                      [("class","classe1")]
                       ) (fmap produtoNome mp) 
    <*> areq doubleField "Preco: " (fmap produtoPreco mp) 

auxProdutoR :: Route App -> Maybe Produto -> Handler Html    
auxProdutoR rt mp = do
    (widget,_) <- generateFormPost (formProduto mp)
    defaultLayout [whamlet|
        <form action=@{rt} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar">
    |]

getProdutoR :: Handler Html
getProdutoR = auxProdutoR ProdutoR Nothing

postProdutoR :: Handler Html
postProdutoR = do
    ((res,_),_) <- runFormPost (formProduto Nothing)
    case res of 
         FormSuccess produto -> do
             pid <- runDB (insert produto)
             redirect (DescR pid)
         _ -> redirect HomeR

-- SELECT * from produto where id = pid
getDescR :: ProdutoId -> Handler Html
getDescR pid = do
    produto <- runDB $ get404 pid
    defaultLayout [whamlet|
        <h1>
            Nome: #{produtoNome produto}
        
        <h2>
            Preço: #{produtoPreco produto}
    |]

-- select * from produto order by preco desc
getListaR :: Handler Html 
getListaR = do 
    -- produtos :: [Entity ProdutoId Produto]
    produtos <- runDB $ selectList [] [Desc ProdutoPreco]
    defaultLayout [whamlet|
        <table>
            <thead>
                <tr>
                    <th>
                        Nome
                    <th>
                        Preco
                    <th>
                    
                    <th>
            
            <tbody>
                $forall Entity pid produto <- produtos
                    <tr>
                        <td>
                            #{produtoNome produto}
                        
                        <td>
                            #{produtoPreco produto}
                        
                        <td>
                            <a href=@{UpdProdR pid}>
                                Editar
                        
                        <td>
                            <form action=@{DelProdR pid} method=post>
                                <input type="submit" value="X">
    |]

getUpdProdR :: ProdutoId -> Handler Html
getUpdProdR pid = do
    antigo <- runDB $ get404 pid 
    auxProdutoR (UpdProdR pid) (Just antigo)

-- UPDATE FROM produto WHERE id = pid SET ...
postUpdProdR :: ProdutoId -> Handler Html
postUpdProdR pid = do
    ((res,_),_) <- runFormPost (formProduto Nothing)
    case res of 
         FormSuccess novo -> do
             _ <- runDB (replace pid novo)
             redirect ListaR
         _ -> redirect HomeR

-- delete from produto where id = pid
postDelProdR :: ProdutoId -> Handler Html
postDelProdR pid = do
    runDB $ delete pid 
    redirect ListaR
    
    
