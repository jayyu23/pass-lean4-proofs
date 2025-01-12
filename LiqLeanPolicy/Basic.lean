-- Basic Account model for the Liq wallet

-- Address is a 32 byte hex value
def Address : Type := String
-- A Liquefaction Account is a shared Account for a group of users
-- Also known as a "domain". The Account.address is the root address of the Account.
structure Account where
  address : Address
  owner : Address

structure SubAccount where
  domain : Account -- The domain or account that this subaccount belongs to
  identifier : String -- The identifier of the subaccount

-- External Transfer
structure ExternalTransfer where
  fromAccount : Account
  toAddress : Address -- The address of the external user receiving the funds
  amount : Nat

-- Internal Transfer
structure InternalTransfer where
  fromSubAccount : SubAccount
  toSubAccount : SubAccount
  amount : Nat

inductive Transfer where
  | external : ExternalTransfer → Transfer
  | internal : InternalTransfer → Transfer

-- Role is a user's role in the system
inductive Role where
  | admin
  | user
  deriving Repr, DecidableEq
