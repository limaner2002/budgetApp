{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where
  -- ( dispDollarAmt
  -- , mkDollar
  -- , dispPeriod
  -- , billPeriodCurrent
  -- , Transaction (..)
  -- , dispTransaction
  -- , User (..)
  -- , readCsv
  -- , displayBookEntries
  -- , _gecuEntry
  -- , _boaEntry
  -- , _amexEntry
  -- ) where

import Data.Decimal
import ClassyPrelude
import Formatting
import Data.Hourglass hiding (format)
import System.Hourglass
import qualified Data.Csv as Csv
import Data.Csv ((.:))
import Control.Lens

newtype User = User
  { _userTxt :: Text
  }
  deriving (Show, IsString, Generic, Eq)

newtype DollarAmt = DollarAmt Decimal
  deriving (Show, Eq, Generic, Num, Ord)

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

data BookEntry = BookEntry
  { _entryCategory :: Maybe Category
  , _entryDescription :: Description
  , _entryAmount :: EntryAmount (NonNegative DollarAmt)
  , _entryDate :: Date
  , _entryId :: EntryId
  , _entryPerson :: User
  , _entryAccount :: Account
  } deriving (Show, Generic)

newtype GECUEntry = GECUEntry
  { _gecuEntry :: EntryId -> User -> AccountNum -> BookEntry
  }

data EntryAmount a
  = Credit a
  | Debit a
  deriving (Show, Generic, Eq, Ord)

newtype EntryId = EntryId
  { _entryIdInt :: Int
  } deriving (Show, Generic, Eq, Ord, Num)

newtype Category = Category
  { _categoryTxt :: Text
  } deriving (Show, Generic, Eq)

newtype NonNegative a = NonNegative
  { _getNonNegative :: a
  } deriving (Show, Generic, Eq, Ord, Num)

newtype Description = Description
  { _descTxt :: Text
  } deriving (Show, Generic, Eq, Ord, Csv.FromField)

instance Csv.FromNamedRecord GECUEntry where
  parseNamedRecord r = do
    desc <- r Csv..: "Description"
    amt <- _gecuAmt <$> Csv.parseNamedRecord r
    dte <- parseDate =<< r Csv..: "Date"
    pure $ GECUEntry $ \eId user acctNum -> BookEntry Nothing desc amt dte eId user (Account "GECU" acctNum)

parseDate :: Monad m => String -> m Date
parseDate = maybe (fail "Could not parse the date!") pure . fmap dtDate . timeParse fmt
  where
    fmt = [Format_Month2, Format_Text '/', Format_Day2, Format_Text '/', Format_Year4]

newtype GECUAmount = GECUAmount
  { _gecuAmt :: EntryAmount (NonNegative DollarAmt)
  } deriving (Show, Generic)

instance Csv.FromNamedRecord GECUAmount where
  parseNamedRecord r = GECUAmount <$> (parseDebit <|> parseCredit)
    where
      parseDebit = Debit . gecuAmount
        <$> r Csv..: "Amount Debit"
      parseCredit = Credit . gecuAmount
        <$> r Csv..: "Amount Credit"

gecuAmount :: Double -> NonNegative DollarAmt
gecuAmount = NonNegative . doubleToDollar . abs

doubleToDollar :: Double -> DollarAmt
doubleToDollar = DollarAmt . roundTo 2 . fromRational . toRational

toEntryAmount :: Double -> EntryAmount (NonNegative DollarAmt)
toEntryAmount amt
  | amt < 0 = Debit $ NonNegative $ doubleToDollar $ abs amt
  | otherwise = Credit $ NonNegative $ doubleToDollar amt

newtype BOAEntry = BOAEntry
  { _boaEntry :: EntryId -> User -> AccountNum -> BookEntry
  }

instance Csv.FromNamedRecord BOAEntry where
  parseNamedRecord r = do
    desc <- r Csv..: "Payee"
    amt <- toEntryAmount <$> r Csv..: "Amount"
    dte <- parseDate =<< r Csv..: "Posted Date"
    pure $ BOAEntry $ \eId user acctNum -> BookEntry Nothing desc amt dte eId user (Account "Bank of America" acctNum)

newtype AmexEntry = AmexEntry
  { _amexEntry :: EntryId -> User -> AccountNum -> BookEntry
  }

instance Csv.FromRecord AmexEntry where
  parseRecord r = do
    desc <- r Csv..! 3
    amt <- toEntryAmount <$> r Csv..! 2
    dte <- parseDate =<< r Csv..! 0
    pure $ AmexEntry $ \eId user acctNum -> BookEntry Nothing desc amt dte eId user (Account "American Express" acctNum)

data Account = Account
  { _actBank :: Text
  , _actNum :: AccountNum
  } deriving (Show, Eq, Ord, Generic)

newtype AccountNum = AccountNum
  { acctNumInt :: Int
  } deriving (Show, Eq, Ord, Num, Generic)

makeLenses ''BookEntry
makeLenses ''GECUEntry
makePrisms ''EntryAmount
makeLenses ''EntryId
makeLenses ''Category
makeLenses ''NonNegative
makeLenses ''Description
makeLenses ''GECUAmount
makeLenses ''BOAEntry
makeLenses ''AmexEntry
makeLenses ''User
