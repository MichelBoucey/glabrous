{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Glabrous.Types where

import           Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap    as KM
#endif
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

newtype Template =
  Template
    { content :: [Token] }
    deriving (Eq, Show, Generic)

instance Serialize Template

newtype Context =
  Context
    { variables :: H.HashMap T.Text T.Text }
    deriving (Eq, Show)

instance ToJSON Context where
  toJSON (Context h) =
#if MIN_VERSION_aeson(2,0,0)
    object (second String <$> KM.toList (KM.fromHashMapText h))
#else
    object (second String <$> H.toList h)
#endif

instance FromJSON Context where
  parseJSON (Object o) = return
#if MIN_VERSION_aeson(2,0,0)
    Context { variables = H.fromList ((\(k,String v) -> (k,v)) <$> H.toList (KM.toHashMapText o)) }
#else
    Context { variables = H.fromList ((\(k,String v) -> (k,v)) <$> H.toList o) }
#endif
  parseJSON _          = fail "expected an object"

data Result
  = Final !T.Text
  | Partial { template :: !Template, context :: !Context }
  deriving (Eq, Show)

