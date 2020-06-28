module BankAccount
  ( BankAccount
  , openAccount
  , closeAccount
  , getBalance
  , incrementBalance
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)

newtype BankAccount = BankAccount
  { balance :: MVar (Maybe Integer)
  }

openAccount :: IO BankAccount
openAccount = BankAccount <$> newMVar (Just 0)

closeAccount :: BankAccount -> IO ()
closeAccount _ = return ()

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readMVar . balance

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance acct val = modifyMVar (balance acct) update
  where
    update curbal = return (newbal, newbal)
      where
        newbal = (val +) <$> curbal
