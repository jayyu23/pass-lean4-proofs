-- Basic Account model for the Liq wallet

-- Address is a 32 byte hex value
structure Address where
  value : String
  deriving DecidableEq, Repr

 instance : ToString Address where
  toString a := a.value

-- A Liquefaction Account is a shared Account for a group of users
-- Also known as a "domain". The Account.address is the root address of the Account.
structure Account where
  address : Address
  owner : Address
  deriving DecidableEq, Repr

structure SubAccount where
  domain : Account -- The domain or account that this subaccount belongs to
  identifier : String -- The identifier of the subaccount
  deriving DecidableEq, Repr

-- External Transfer
structure ExternalTransfer where
  fromAccount : Account
  toAddress : Address -- The address of the external user receiving the funds
  amount : Nat
  deriving DecidableEq, Repr

-- Internal Transfer
structure InternalTransfer where
  fromSubAccount : SubAccount
  toSubAccount : SubAccount
  amount : Nat
  deriving DecidableEq, Repr
