{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Implantation where

import Import

formImplantation :: Form (Implantation, Text)
formImplantation = undefined

getImplantationR :: Maybe PatientId -> Handler Html
getImplantationR = undefined

postImplantationR :: PatientId -> Handler Html
postImplantationR = undefined

postIDeleteR :: ImplantationId -> Handler Html
postIDeleteR = undefined