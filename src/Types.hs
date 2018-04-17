{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( dispDollarAmt
  , mkDollar
  , mkPeriod
  , dispPeriod
  , billPeriodCurrent
  , Transaction (..)
  , dispTransaction
  ) where

import Data.Decimal
import ClassyPrelude
import Formatting
import Data.Hourglass hiding (format)
import System.Hourglass

newtype User = User Text

newtype DollarAmt = DollarAmt Decimal
  deriving Show

dispDollarAmt :: DollarAmt -> Text
dispDollarAmt (DollarAmt amt) = toStrict $ format ("$" % commas % "." % int) intPart decPart
  where
    intPart :: Integer
    intPart = truncate amt
    decPart :: Integer
    decPart = decimalMantissa amt `rem` 10 ^ (decimalPlaces amt)

mkDollar :: Integer -> DollarAmt
mkDollar = DollarAmt . Decimal 2

newtype BillPeriod = BillPeriod Date
  deriving Show

mkPeriod :: Int -> Month -> BillPeriod
mkPeriod n mth = BillPeriod $ Date n mth 1

billPeriodCurrent :: IO BillPeriod
billPeriodCurrent = BillPeriod . timeGetDate <$> timeCurrent

dispPeriod :: BillPeriod -> Text
dispPeriod (BillPeriod date) = pack $ timePrint billPeriodFormat date
  where
    billPeriodFormat = [Format_MonthName_Short, Format_Text ' ', Format_Year]

data Transaction = Transaction
  { _transAmt :: DollarAmt
  , _transDesc :: Text
  , _transPeriod :: BillPeriod
  } deriving Show

dispTransaction :: Transaction -> Text
dispTransaction (Transaction amt desc period) =
  desc <> ": " <> dispDollarAmt amt <> " for the period of " <> dispPeriod period
