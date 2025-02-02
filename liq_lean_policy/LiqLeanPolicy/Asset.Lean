import Mathlib.Data.Finmap
import LiqLeanPolicy.EVMState

inductive AssetType
  | Ether -- ETH is a special asset case
  | Token -- Non-ETH Token contract
  | GSM -- General Signable Message
  deriving Repr, BEq, DecidableEq

structure Asset where
  id : String
  assetType : AssetType
  balanceMap : List (Address × Nat)
  deriving Repr, BEq, DecidableEq

inductive TransactionType
  | external
  | internal
  deriving Repr, BEq, DecidableEq

structure PassTransaction where
  txType : TransactionType
  sender : Address
  recipient : Address
  asset : Asset
  amount : Nat
  deriving Repr, BEq, DecidableEq

def Asset.getBalance (asset : Asset) (address : Address) : Nat :=
  match asset.balanceMap.lookup address with
  | some bal => bal
  | none => 0

def Asset.getTotalBalance (asset : Asset) : Nat :=
  asset.balanceMap.foldl (fun acc (addr, bal) => acc + bal) 0
