module BankAccount ( BankAccount, openAccount, closeAccount
                   , getBalance, incrementBalance ) where

data BankAccount = BankAccount

openAccount :: IO BankAccount
openAccount = undefined

closeAccount :: BankAccount -> IO ()
closeAccount = undefined

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = undefined

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance = undefined
