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