{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Usuario where

import Import
import Network.HTTP.Types.Status
import Database.Persist.MySQL
import qualified Data.Aeson as J
import qualified Data.Aeson.Parser as JP
import Data.Conduit.Attoparsec (sinkParser) 


postAlterarObjetivo :: Handler TypedContent
postAlterarObjetivo = do
    json <- requireJsonBody :: Handler Value
    unpUsuarioid <- requireJsonKey "id_usuario" json
    usuarioid <- (requireJsonParse unpUsuarioid :: Handler Tb_usuarioId)
    _ <- runDB $ get404 usuarioid
    unpObjetivo <- requireJsonKey "cd_objetivo" json
    unpMeta <- requireJsonKey "vl_objetivo_kg" json
    objetivo <- (requireJsonParse unpObjetivo :: Handler Text)
    meta <- (requireJsonParse unpMeta :: Handler Double)
    runDB $ update usuarioid [Tb_usuarioCd_objetivo =. objetivo, Tb_usuarioVl_objetivo_kg =. meta]
    sendStatusJSON ok200 (object ["success" .= True])

postAlterarPesoAltura :: Handler TypedContent
postAlterarPesoAltura = do
    json <- requireJsonBody :: Handler Value
    unpUsuarioid <- requireJsonKey "id_usuario" json
    usuarioid <- (requireJsonParse unpUsuarioid :: Handler Tb_usuarioId)
    _ <- runDB $ get404 usuarioid
    unpPeso <- requireJsonKey "peso_kg" json
    unpAltura <- requireJsonKey "altura_m" json
    peso <- (requireJsonParse unpPeso :: Handler Double)
    altura <- (requireJsonParse unpAltura :: Handler Double)
    runDB $ update usuarioid [Tb_usuarioPeso_kg =. peso, Tb_usuarioAltura_m =. altura]
    sendStatusJSON ok200 (object ["success" .= True])
    

postAlterarDados :: Handler TypedContent
postAlterarDados = do
        json <- requireJsonBody :: Handler Value
        unpUsuarioid <- requireJsonKey "id_usuario" json
        usuarioid <- (requireJsonParse unpUsuarioid :: Handler Tb_usuarioId)
        _ <- runDB $ get404 usuarioid
        unpNome <- requireJsonKey "nome" json
        unpTelefone <- requireJsonKey "telefone" json
        unpSexo <- requireJsonKey "sexo" json
        unpDtNasc <- requireJsonKey "dt_nasc" json
        unpPeso <- requireJsonKey "peso_kg" json
        unpAltura <- requireJsonKey "altura_m" json
        nome <- (requireJsonParse unpNome :: Handler Text)
        telefone <- (requireJsonParse unpTelefone :: Handler Text)
        sexo <- (requireJsonParse unpSexo :: Handler Text)
        dtNasc <- (requireJsonParse unpDtNasc :: Handler Text)
        peso <- (requireJsonParse unpPeso :: Handler Double)
        altura <- (requireJsonParse unpAltura :: Handler Double)
        runDB $ update usuarioid [Tb_usuarioNome =. nome, Tb_usuarioTelefone =. telefone, Tb_usuarioSexo =. sexo, Tb_usuarioDt_nasc =. dtNasc, Tb_usuarioPeso_kg =. peso, Tb_usuarioAltura_m =. altura]
        sendStatusJSON ok200 (object ["success" .= True])

    
-- PEGAR CHAVE ESPECÍFICA DO JSON
requireJsonKey :: (MonadHandler m) => Text -> Value -> m Value
requireJsonKey key jObject@(Object hashMap) = case lookup key hashMap of
                                    Nothing -> invalidArgs ["Couldn't find a value when looking up the key " <> key <> " in the object: " <> (pack (show jObject))]
                                    Just v -> return v
requireJsonKey key invalid = invalidArgs ["When looking up the key " <> key <> ", expected an object but got a " ++ (pack (show invalid))]

-- PARSEAR O JSON
requireJsonParse :: (MonadHandler m, FromJSON a) => Value -> m a
requireJsonParse v = case J.fromJSON v of
  J.Error s -> invalidArgs [pack s]
  J.Success a -> return a