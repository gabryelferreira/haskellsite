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

getPratoById :: Int -> Int -> Handler TypedContent
getPratoById idPrato idUsuario= do 
    prato <- runDB $ selectList [Tb_pratoId_prato ==. idPrato, Tb_pratoId_usuario ==. idUsuario] []
    idAlimentos <- return $ fmap (tb_pratoId_alimento . entityVal) prato
    soma <- runDB $ selectList [Tb_alimentoId_alimento <-. idAlimentos ] []
    let qtd_alimento = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + 1 ) (0::Int) prato
    let peso = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoQuantidade prato ) (0) soma
    let somaPot = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoPotassio prato ) (0) soma
    let somazinco = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoZinco prato ) (0) soma
    let somavitamina_c = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoVitamina_c prato ) (0) soma
    let somasodio = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoSodio prato ) (0) soma
    let somaproteina = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoProteina prato ) (0) soma
    let somamanganes = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoManganes prato ) (0) soma
    let somamagnesio = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoMagnesio prato ) (0) soma
    let somalipideos = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoLipideos prato ) (0) soma
    let somakcal = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoKcal prato ) (0) soma
    let somafosforo = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoFosforo prato ) (0) soma
    let somafibra_alimentar = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoFibra_alimentar prato ) (0) soma
    let somaferro = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoFerro prato ) (0) soma
    let somacobre = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoCobre prato ) (0) soma
    let somacarboidrato = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoCarboidrato prato ) (0) soma
    let somacalcio = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_alimentoCalcio prato ) (0) soma
    idAlimentos <- return $ fmap (tb_pratoId_alimento . entityVal) prato
    prato_feito <- runDB $ selectList [Tb_prato_feitoId_prato ==. idPrato ] []
    let dt_consumo = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum ++ tb_prato_feitoDt_consumo prato ) ("") prato_feito
    let id_prato_feito = Import.foldr (\ (Entity _ prato) valorAcum -> valorAcum + tb_prato_feitoId_prato_feito prato ) (0) prato_feito
    sendStatusJSON ok200 (object ["result" .= object["id_prato_feito" .= id_prato_feito, "id_prato" .= idPrato, "dt_consumo" .= dt_consumo, "qtd_alimentos".= qtd_alimento, "peso".= peso,"kcal_consumo" .= somakcal, "proteina_consumo".= somaproteina, "lipideos_consumo".= somalipideos ,"carboidrato_consumo" .=somacarboidrato,"magnesio_consumo" .= somamagnesio, "manganes_consumo" .= somamanganes, "fosforo_consumo" .= somafosforo, "ferro_consumo" .= somaferro, "sodio_consumo".= somasodio, "potassio_consumo" .= somaPot, "cobre_consumo" .= somacobre, "zinco_consumo" .= somazinco, "vitamina_c_consumo" .= somavitamina_c, "success" .= True]])
    
    
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