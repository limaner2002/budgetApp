{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Types
import Tupleable
import Database
import ProjectM36.Client.Simple
import ProjectM36.Tupleable
import Control.Lens

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

displayBookEntries :: (ByteString -> Either String (Vector a)) -> (a -> EntryId -> User -> BookEntry) -> (Vector BookEntry -> IO b) -> Int -> User -> FilePath -> IO b
displayBookEntries decodeFcn f cont startIdx user path = do
  eAs <- readCsv decodeFcn path
  case eAs of
    Left error -> fail error
    Right as -> do
      let ids = fromList $ fmap EntryId $ [startIdx .. startIdx + length as]
          entries = zipWith (\a id -> f a id user) as ids
      cont entries

insertBookEntries :: (ByteString -> Either String (Vector a)) -> (a -> EntryId -> User -> BookEntry) -> ConnectionInfo -> User -> FilePath -> IO (Either DbError (Either RelationalError ()))
insertBookEntries decodeFcn f connInfo user path = do
  res <- runTransaction connInfo $ retrieveTupleable (RelationVariable "bookEntries" ())
  case maximumMay $ res ^.. _Right . traverse . _Right . to _entryId of
    Nothing -> displayBookEntries decodeFcn f (insertFcn connInfo) 1 user path
    Just (EntryId n) -> displayBookEntries decodeFcn f (insertFcn connInfo) (n + 1) user path

insertFcn :: (Tupleable a, Traversable t) => ConnectionInfo -> t a -> IO (Either DbError (Either RelationalError ()))
insertFcn connInfo l = runTransaction connInfo $ mapM execute $ toInsertExpr l "bookEntries"
