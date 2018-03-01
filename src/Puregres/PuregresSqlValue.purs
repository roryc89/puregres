module Puregres.PuregresSqlValue where

import Prelude

import Data.Date (year, month, day)
import Data.DateTime (DateTime(DateTime))
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Nullable (toNullable)
import Data.Time (hour, minute, second)
import Database.Postgres.SqlValue (SqlValue)
import Unsafe.Coerce (unsafeCoerce)

class IsSqlValue a where
  toSql :: a -> SqlValue

instance isSqlValueString :: IsSqlValue String where
  toSql = unsafeCoerce

instance isSqlValueNumber :: IsSqlValue Number where
  toSql = unsafeCoerce

instance isSqlValueInt :: IsSqlValue Int where
  toSql = unsafeCoerce <<< toNumber

instance isSqlValueBoolean :: IsSqlValue Boolean where
  toSql = unsafeCoerce

instance isSqlValueMaybe :: (IsSqlValue a) => IsSqlValue (Maybe a) where
  toSql = unsafeCoerce <<< toNullable <<< (toSql <$> _)

instance isSqlValueArray :: (IsSqlValue a) => IsSqlValue (Array a) where
  toSql = unsafeCoerce <<<  (toSql <$> _)

instance isSqlValueDateTime :: IsSqlValue DateTime where
  toSql = toSql <<< format
    where
      format (DateTime d t)
        = show (fromEnum (year d)) <> "-"
        <> zeroPad (fromEnum (month d)) <> "-"
        <> zeroPad (fromEnum (day d)) <> " "
        <> zeroPad (fromEnum (hour t)) <> ":"
        <> zeroPad (fromEnum (minute t)) <> ":"
        <> zeroPad (fromEnum (second t))

      zeroPad :: Int -> String
      zeroPad i | i < 10 = "0" <> (show i)
      zeroPad i = show i
