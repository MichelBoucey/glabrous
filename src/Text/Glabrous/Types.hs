{-# LANGUAGE OverloadedStrings #-}

module Text.Glabrous.Types where

import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T

data Token = Tag T.Text | Literal T.Text deriving (Show,Eq)

data Template = Template { content :: ![Token] } deriving (Show,Eq)

data Context = Context { variables :: H.HashMap T.Text T.Text } deriving (Show,Eq)

instance ToJSON Context where
    toJSON (Context h) = object $ (\(k,v) -> (k,String v)) <$> H.toList h

instance FromJSON Context where
    parseJSON (Object o) = return
        Context { variables =
                      H.fromList $ (\(k,String v) -> (k,v)) <$> H.toList o
                }
    parseJSON _          = fail "expected an object"

type Tag = T.Text

data Result = Final T.Text | Partial { template :: !Template, tags :: ![Tag]}  deriving (Show,Eq)

