{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Ajuda where

import Import
import Network.HTTP.Types.Status
import Database.Persist.MySQL

-- funcao para enviar uma duvida
postContato :: Handler TypedContent
postContato = do
     ajuda <- requireJsonBody :: Handler Tb_ajuda
     _ <- runDB $ insert ajuda
     sendStatusJSON ok200 (object ["success" .= True])