-- registeredUser1.hs
module Chapter07.Content.RegisteredUser1 where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber
