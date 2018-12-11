{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Alimento where

import Import
import Network.HTTP.Types.Status
import Database.Persist.MySQL

-- funcao para sugerir um alimento
postSugerirAlimento :: Handler TypedContent
postSugerirAlimento = do
     alimento <- requireJsonBody :: Handler Tb_sugestao_alimento
     _ <- runDB $ insert alimento
     sendStatusJSON ok200 (object ["success" .= True])