{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Types

main :: IO ()
main = do
  period <- billPeriodCurrent

  let t = Transaction (mkDollar 123456) "Example transaction" period "Josh"

  putStrLn $ dispTransaction t
  -- Prints 'User "Josh" has the transaction
  --         Example transaction: $1,234.56 for the period of Apr 2018'
