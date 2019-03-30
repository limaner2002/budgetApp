{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( dispDollarAmt
  , mkDollar
  , dispPeriod
  , billPeriodCurrent
  , Transaction (..)
  , dispTransaction
  , User (..)
  , readCsv
  , displayBookEntries
  , _gecuEntry
  , _boaEntry
  , _amexEntry
  ) where

import Data.Decimal
import ClassyPrelude
import Formatting
import Data.Hourglass hiding (format)
import System.Hourglass
import qualified Data.Csv as Csv
import Data.Csv ((.:))
import Data.Proxy

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

readCsv :: (ByteString -> Either String (Vector a)) -> FilePath -> IO (Either String (Vector a))
readCsv decodeFcn path = do
  content <- readFile path
  pure $ decodeFcn content

displayBookEntries :: (ByteString -> Either String (Vector a)) -> (a -> EntryId -> User -> BookEntry) -> Int -> User -> FilePath -> IO ()
displayBookEntries decodeFcn f startIdx user path = do
  eAs <- readCsv decodeFcn path
  case eAs of
    Left error -> putStrLn $ pack error
    Right as -> do
      let ids = fromList $ fmap EntryId $ [startIdx .. startIdx + length as]
          entries = zipWith (\a id -> f a id user) as ids
      mapM_ print entries

data BookEntry = BookEntry
  { _entryCategory :: Maybe Category
  , _entryDescription :: Description
  , _entryAmount :: EntryAmount (NonNegative DollarAmt)
  , _entryDate :: Date
  , _entryId :: EntryId
  , _entryPerson :: User
  } deriving (Show, Generic)

newtype GECUEntry = GECUEntry
  { _gecuEntry :: EntryId -> User -> BookEntry
  }

data EntryAmount a
  = Credit a
  | Debit a
  deriving (Show, Generic, Eq)

newtype EntryId = EntryId
  { _entryIdInt :: Int
  } deriving (Show, Generic, Eq)

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
  parseNamedRecord r = GECUEntry
    <$> ( BookEntry
          <$> pure Nothing
          <*> r Csv..: "Description"
          <*> (_gecuAmt <$> Csv.parseNamedRecord r)
          <*> (parseDate =<< r Csv..: "Date")
        )

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
  { _boaEntry :: EntryId -> User -> BookEntry
  }

instance Csv.FromNamedRecord BOAEntry where
  parseNamedRecord r = BOAEntry
    <$> ( BookEntry
      <$> pure Nothing
      <*> r Csv..: "Payee"
      <*> (toEntryAmount <$> r Csv..: "Amount")
      <*> (parseDate =<< r Csv..: "Posted Date")
        )

newtype AmexEntry = AmexEntry
  { _amexEntry :: EntryId -> User -> BookEntry
  }

instance Csv.FromRecord AmexEntry where
  parseRecord r = AmexEntry
    <$> ( BookEntry
        <$> pure Nothing
        <*> r Csv..! 2
        <*> (toEntryAmount <$> r Csv..! 7)
        <*> (parseDate =<< r Csv..! 0)
        )
