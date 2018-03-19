module Puregres.PostgresType where

import Prelude

import Data.Maybe (Maybe(..))

class PostgresType t where
  postgresType :: t -> String
  postgresDefault :: t -- can be anything - only used to figure out postgresType for type vars

instance postgresTypeInt :: PostgresType Int where
  postgresType _ = "int"
  postgresDefault = 0

instance postgresTypeString :: PostgresType String where
  postgresType _ = "varchar"
  postgresDefault = ""

instance postgresTypeNumber :: PostgresType Number where
  postgresType _ = "numeric"
  postgresDefault = 0.0

instance postgresTypeBoolean :: PostgresType Boolean where
  postgresType _ = "boolean"
  postgresDefault = false

instance postgresTypeArray :: (PostgresType a) => PostgresType (Array a) where
  postgresType _ =  postgresType (postgresDefault :: a) <> "[]"
  postgresDefault = []

instance postgresTypeMaybe :: (PostgresType a) => PostgresType (Maybe a) where
  postgresType _ =  postgresType (postgresDefault :: a)
  postgresDefault = Nothing
