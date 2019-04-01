{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Database where

import ClassyPrelude
import ProjectM36.Client.Simple
import ProjectM36.Base
import ProjectM36.Tupleable
import ProjectM36.Relation hiding (toList)

retrieveTupleable :: Tupleable a => RelationalExpr -> Db [Either RelationalError a]
retrieveTupleable expr = fmap fromTuple . relFold cons mempty <$> query expr

withDatabase :: ConnectionInfo -> (DbConn -> IO a) -> IO (Either DbError a)
withDatabase conInfo = bracket connect disconnect . traverse
  where
    connect = simpleConnectProjectM36 conInfo

disconnect :: Either DbError DbConn -> IO (Either DbError ())
disconnect (Left msg) = pure $ Left msg
disconnect (Right conn) = Right <$> close conn

runTransaction :: ConnectionInfo -> Db a -> IO (Either DbError a)
runTransaction conInfo = fmap ClassyPrelude.join . withDatabase conInfo . flip withTransaction

crashSafeConnInfo :: FilePath -> ConnectionInfo
crashSafeConnInfo path = InProcessConnectionInfo (CrashSafePersistence path) emptyNotificationCallback []

