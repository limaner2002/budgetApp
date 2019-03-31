{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tupleable where

import ClassyPrelude
import ProjectM36.Tupleable
import ProjectM36.Atomable
import ProjectM36.Base
import Types
import Data.Binary
import Data.Decimal
import Data.Proxy
import Data.Hourglass

instance Atomable a => Atomable (EntryAmount a)
instance NFData a => NFData (EntryAmount a)
instance Binary a => Binary (EntryAmount a)

instance Atomable a => Atomable (NonNegative a)
instance NFData a => NFData (NonNegative a)
instance Binary a => Binary (NonNegative a)

instance Atomable Category
instance NFData Category
instance Binary Category

instance Atomable Description
instance NFData Description
instance Binary Description

instance Atomable DollarAmt
instance NFData DollarAmt
instance Binary DollarAmt

instance (Atomable a, Integral a) => Atomable (DecimalRaw a) where
  toAtom d = ConstructedAtom "DecimalRaw" (toAtomType (Proxy :: Proxy (DecimalRaw a))) [ByteStringAtom $ toStrict $ encode d]

  fromAtom (ConstructedAtom "DecimalRaw" _ [ByteStringAtom bs]) = decode $ fromStrict bs
  fromAtom _ = error "Could not decode 'DecimalRaw' Atom"

  toAtomType _ = ConstructedAtomType "DecimalRaw" mempty

  toAddTypeExpr _ = AddTypeConstructor (ADTypeConstructorDef "DecimalRaw" []) [DataConstructorDef "DecimalRaw" [DataConstructorDefTypeConstructorArg (PrimitiveTypeConstructor "ByteString" ByteStringAtomType)]]

instance Binary a => Binary (DecimalRaw a) where
  put d = do
    put $ decimalPlaces d
    put $ decimalMantissa d

  get = do
    places <- get
    mantissa <- get
    pure $ Decimal places mantissa

monthBS :: Month -> ByteString
monthBS January = "January"
monthBS February = "February"
monthBS March = "March"
monthBS April = "April"
monthBS May = "May"
monthBS June = "June"
monthBS July = "July"
monthBS August = "August"
monthBS September = "September"
monthBS October = "October"
monthBS November = "November"
monthBS December = "December"

instance Binary Month where
  put = put . monthBS

  get = do
    (bs :: Text) <- get
    case readMay bs of
      Nothing -> fail "Could not decode 'Month'"
      Just mth -> pure mth

instance Binary Date where
  put date = do
    put $ dateYear date
    put $ dateMonth date
    put $ dateDay date

  get = do
    year <- get
    month <- get
    day <- get
    pure $ Date year month day

instance Atomable Date where
  toAtom d = ConstructedAtom "Date" (toAtomType (Proxy :: Proxy Date)) [ByteStringAtom $ toStrict $ encode d]

  fromAtom (ConstructedAtom "Date" _ [ByteStringAtom bs]) = decode $ fromStrict bs
  fromAtom _ = error "Could not decode 'Date' Atom"

  toAtomType _ = ConstructedAtomType "Date" mempty

  toAddTypeExpr _ = AddTypeConstructor (ADTypeConstructorDef "Date" []) [DataConstructorDef "Date" [DataConstructorDefTypeConstructorArg (PrimitiveTypeConstructor "ByteString" ByteStringAtomType)]]

instance Atomable EntryId
instance NFData EntryId
instance Binary EntryId

instance Atomable User
instance NFData User
instance Binary User

instance Tupleable BookEntry

schema :: [DatabaseContextExpr]
schema = toAddTypeExpr (Proxy :: Proxy (EntryAmount (NonNegative DollarAmt)))
  : toAddTypeExpr (Proxy :: Proxy (NonNegative DollarAmt))
  : toAddTypeExpr (Proxy :: Proxy Category)
  : toAddTypeExpr (Proxy :: Proxy Description)
  : toAddTypeExpr (Proxy :: Proxy DollarAmt)
  : toAddTypeExpr (Proxy :: Proxy Decimal)
  : toAddTypeExpr (Proxy :: Proxy Date)
  : toAddTypeExpr (Proxy :: Proxy EntryId)
  : toAddTypeExpr (Proxy :: Proxy User)
  : toDefineExpr (Proxy :: Proxy BookEntry) "bookEntries"
  : []