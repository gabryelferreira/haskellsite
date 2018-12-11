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

-- select alimentos por nome, com offset e limit para fazer infinite scroll
getListAlimentoByNome :: Text -> Int -> Int -> Handler TypedContent
getListAlimentoByNome nome offset limit = do 
     alimentos <- runDB $ selectList [Filter Tb_alimentoNome (Left $  concat ["%", nome, "%"]) (BackendSpecificFilter "LIKE")] [LimitTo limit, OffsetBy offset]
     sendStatusJSON ok200 (object ["success" .= True, "result" .= alimentos])