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
  asset.balanceMap.foldl (fun sum (_, bal) => sum + bal) 0

def Asset.updateBalance (asset : Asset) (address : Address) (amount : Nat) : Asset :=
  let newBalanceEntry := (address, amount)
  let updatedBalanceMap :=
    match asset.balanceMap.lookup address with
    | some _ =>  -- Address exists, update the balance
      asset.balanceMap.map (fun entry =>
        if entry.1 == address then newBalanceEntry else entry)
    | none => -- Address doesn't exist, add a new entry
      newBalanceEntry :: asset.balanceMap
  { asset with balanceMap := updatedBalanceMap }

def Asset.hasBalance (asset : Asset) (address : Address) (amount : Nat) : Bool :=
  asset.getBalance address >= amount
