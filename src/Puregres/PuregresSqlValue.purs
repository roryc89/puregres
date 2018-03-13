module Puregres.PuregresSqlValue where

import Prelude

import Control.Monad.Except (except)
import Data.Date (canonicalDate, day, month, year)
import Data.DateTime (DateTime(DateTime), Time(..))
import Data.Either (note)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foreign (F, Foreign, ForeignError(ForeignError), fail, readArray, readNull)
import Data.Foreign.Class (class Decode, decode)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe)
import Data.Nullable (toNullable)
import Data.String (Pattern(..), split)
import Data.Time (hour, minute, second)
import Data.Traversable (traverse)
import Database.Postgres.SqlValue (SqlValue)
import Unsafe.Coerce (unsafeCoerce)

class IsSqlValue a where
  toSql :: a -> SqlValue
  decode_ :: Foreign -> F a

instance isSqlValueString :: IsSqlValue String where
  toSql = unsafeCoerce
  decode_ = decode

instance isSqlValueNumber :: IsSqlValue Number where
  toSql = unsafeCoerce
  decode_ = decode

instance isSqlValueInt :: IsSqlValue Int where
  toSql = unsafeCoerce <<< toNumber
  decode_ = decode

instance isSqlValueBoolean :: IsSqlValue Boolean where
  toSql = unsafeCoerce
  decode_ = decode

instance isSqlValueMaybe :: (IsSqlValue a, Decode a) => IsSqlValue (Maybe a) where
  toSql = unsafeCoerce <<< toNullable <<< (toSql <$> _)
  decode_ f = readNull f >>= traverse decode

instance isSqlValueArray :: (IsSqlValue a, Decode a) => IsSqlValue (Array a) where
  toSql = unsafeCoerce <<< (toSql <$> _)
  decode_ f = readArray f >>= traverse decode

instance isSqlValueDateTime :: IsSqlValue DateTime where
  decode_ f = do
     decoded :: String <- decode f
     parsed :: DateTime <- parse decoded
     pure parsed
    where
      parse :: String -> F DateTime
      parse string =
        case split (Pattern " ") string of
          [date, time] ->
            case [split (Pattern "-") date, split (Pattern ":") time] of
              [[year, month, day], [hour, minute, second]] ->
                 decodeDateTimeParts year month day hour minute second

              _ -> fail $ ForeignError "date or time parts not matched"

          _ ->  fail $ ForeignError "date and time not matched"

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

decodeDateTimeParts :: String -> String -> String -> String -> String -> String -> F DateTime
decodeDateTimeParts yearStr monthStr dayStr hourStr minuteStr secondStr = do
  year <- strToEnum yearStr
  month <- strToEnum monthStr
  day <- strToEnum dayStr
  hour <- strToEnum hourStr
  minute <- strToEnum minuteStr
  second <- strToEnum secondStr
  pure $ DateTime
   (canonicalDate year month day)
   (Time hour minute second bottom)

strToEnum :: forall a. Bounded a => BoundedEnum a => String -> F a
strToEnum a = fromString a >>= toEnum
  # note (pure $ ForeignError $ "could not convert string to enum: " <> a)
  # except
