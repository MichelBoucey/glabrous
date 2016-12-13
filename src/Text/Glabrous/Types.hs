{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Glabrous.Types where

import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import           Data.Serialize
import           Data.Serialize.Text ()
import           Control.Arrow       (second)
import           GHC.Generics

data Token
  = Tag !T.Text
  | Literal !T.Text
  deriving (Eq, Show, Generic)

instance Serialize Token

data Template =
  Template
    { content :: ![Token] }
    deriving (Eq, Show, Generic)

instance Serialize Template

data Context =
  Context
    { variables :: H.HashMap T.Text T.Text }
    deriving (Eq, Show)

instance ToJSON Context where
  toJSON (Context h) =
    object (second String <$> H.toList h)

instance FromJSON Context where
  parseJSON (Object o) = return
    Context { variables = H.fromList $ (\(k,String v) -> (k,v)) <$> H.toList o }
  parseJSON _         = fail "expected an object"

type Tag = T.Text

data Result
  = Final !T.Text
  | Partial { template :: !Template , context :: !Context }
  deriving (Eq, Show)

