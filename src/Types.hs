{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Types
  ( dispDollarAmt
  , mkDollar
  , dispPeriod
  , billPeriodCurrent
  , Transaction (..)
  , dispTransaction
  , User (..)
  , readCsv
  , displayTransactions
  ) where

import Data.Decimal
import ClassyPrelude
import Formatting
import Data.Hourglass hiding (format)
import System.Hourglass
import qualified Data.Csv as Csv
import Data.Csv ((.:))

newtype User = User Text
  deriving (Show, IsString)

newtype DollarAmt = DollarAmt Decimal
  deriving Show

instance Csv.FromField DollarAmt where
  parseField bs = do
    res <- Csv.parseField bs
    pure $ mkDollar res

dispDollarAmt :: DollarAmt -> Text
dispDollarAmt (DollarAmt amt) = toStrict $ format ("$" % commas % "." % int) intPart decPart
  where
    intPart :: Integer
    intPart = truncate amt
    decPart :: Integer
    decPart = decimalMantissa amt `rem` 10 ^ (decimalPlaces amt)

mkDollar :: Integer -> DollarAmt
mkDollar = DollarAmt . Decimal 2

newtype BillPeriod = BillPeriod DateTime

instance Show BillPeriod where
  show = unpack . dispPeriod

instance Csv.FromField BillPeriod where
  parseField bs = case parsedTime of
    Nothing -> fail $ "The value " <> show bs <> " is not a valid Billing Period."
    Just period -> pure $ BillPeriod period
    where
      parsedTime = timeParse billPeriodFormat str
      str = unpack $ decodeUtf8 bs

billPeriodCurrent :: IO BillPeriod
billPeriodCurrent = BillPeriod . timeGetDateTimeOfDay <$> timeCurrent

dispPeriod :: BillPeriod -> Text
dispPeriod (BillPeriod date) = pack $ timePrint billPeriodFormat date

billPeriodFormat = [Format_MonthName_Short, Format_Text ' ', Format_Year]

data Transaction = Transaction
  { _transAmt :: DollarAmt
  , _transDesc :: Text
  , _transPeriod :: BillPeriod
  , _transUser :: User
  } deriving Show

dispTransaction :: Transaction -> Text
dispTransaction (Transaction amt desc period user) =
  tshow user <> " has the transaction\n"
  <> desc <> ": " <> dispDollarAmt amt <> " for the period of " <> dispPeriod period

instance Csv.FromNamedRecord (User -> Transaction) where
  parseNamedRecord r = Transaction
    <$> r .: "Amount"
    <*> r .: "Description"
    <*> r .: "Period"

readCsv :: User -> FilePath -> IO (Either String (Vector Transaction))
readCsv user@(User username) filePrefix = do
  content <- fromStrict <$> (readFile $ filePrefix <> unpack username <> "-Table 1.csv")
  let funcs = fmap snd $ Csv.decodeByName content :: Either String (Vector (User -> Transaction))
  pure $ fmap (fmap (\f -> f user)) funcs

displayTransactions :: User -> FilePath -> IO ()
displayTransactions user path = do
  eTransactions <- readCsv user path
  case eTransactions of
    Left error -> putStrLn $ pack error
    Right transactions -> mapM_ print transactions
