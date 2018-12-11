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