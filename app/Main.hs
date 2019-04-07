{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import ClassyPrelude
import Types
import Tupleable
import Database
import ProjectM36.Client.Simple
import ProjectM36.Tupleable
import Control.Lens
import qualified Control.Foldl as F
import Time.Types

main :: IO ()
main = do
  period <- billPeriodCurrent

  let t = Transaction (mkDollar 123456) "Example transaction" period "Josh"

  putStrLn $ dispTransaction t
  -- Prints 'User "Josh" has the transaction
  --         Example transaction: $1,234.56 for the period of Apr 2018'

readCsv :: (ByteString -> Either String (Vector a)) -> FilePath -> IO (Either String (Vector a))
readCsv decodeFcn path = do
  content <- readFile path
  pure $ decodeFcn content

displayBookEntries :: (ByteString -> Either String (Vector a)) -> (a -> EntryId -> User -> AccountNum -> BookEntry) -> (Vector BookEntry -> IO b) -> Int -> User -> AccountNum -> FilePath -> IO b
displayBookEntries decodeFcn f cont startIdx user accountNum path = do
  eAs <- readCsv decodeFcn path
  case eAs of
    Left error -> fail error
    Right as -> do
      let ids = fromList $ fmap EntryId $ [startIdx .. startIdx + length as]
          entries = zipWith (\a id -> f a id user accountNum) as ids
      cont entries

insertBookEntries :: (ByteString -> Either String (Vector a)) -> (a -> EntryId -> User -> AccountNum -> BookEntry) -> ConnectionInfo -> User -> AccountNum -> FilePath -> IO (Either DbError (Either RelationalError ()))
insertBookEntries decodeFcn f connInfo user accountNum path = do
  res <- runTransaction connInfo $ retrieveTupleable (RelationVariable "bookEntries" ())
  case maximumMay $ res ^.. _Right . traverse . _Right . to _entryId of
    Nothing -> displayBookEntries decodeFcn f (insertFcn connInfo) 1 user accountNum path
    Just (EntryId n) -> displayBookEntries decodeFcn f (insertFcn connInfo) (n + 1) user accountNum path

insertFcn :: (Tupleable a, Traversable t) => ConnectionInfo -> t a -> IO (Either DbError (Either RelationalError ()))
insertFcn connInfo l = runTransaction connInfo $ mapM execute $ toInsertExpr l "bookEntries"

monthlySum :: Fold BookEntry (NonNegative DollarAmt) -> [BookEntry] -> Map Month (NonNegative DollarAmt)
monthlySum fld entries = F.fold monthlyFold entryTuples
  where
    entryTuples = entries ^.. traverse . runFold ((,) <$> Fold fld <*> Fold entryDate)
    monthlyFold = F.groupBy (dateMonth . snd) (F.premap fst F.sum)

monthlyDebit :: [BookEntry] -> Map Month (NonNegative DollarAmt)
monthlyDebit = monthlySum (entryAmount . _Debit)

monthlyCredit :: [BookEntry] -> Map Month (NonNegative DollarAmt)
monthlyCredit = monthlySum (entryAmount . _Credit)

runningSum :: F.Fold BookEntry DollarAmt
runningSum = F.premap (view entryAmount) $ F.Fold runningSumStep 0 id

runningSumStep :: DollarAmt -> EntryAmount (NonNegative DollarAmt) -> DollarAmt
runningSumStep total (Debit amt) = total - amt ^. getNonNegative
runningSumStep total (Credit amt) = total + amt ^. getNonNegative

netMonthlyF :: F.Fold (BookEntry, Date) (Map Month DollarAmt)
netMonthlyF = F.groupBy (dateMonth . snd) (F.premap fst runningSum)

netMonthly :: [BookEntry] -> Map Month DollarAmt
netMonthly entries = F.fold netMonthlyF $ entries ^.. traverse . runFold ((,) <$> Fold id <*> Fold entryDate)
