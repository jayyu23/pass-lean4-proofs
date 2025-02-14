import LiqLeanPolicy.EVMState

inductive AssetType
  | Undefined
  | Ether -- ETH is a special asset case
  | Token -- Non-ETH Token contract
  | GSM -- General Signable Message
  deriving Repr, BEq, DecidableEq

structure Asset where
  id : String
  assetType : AssetType
  balanceMap : Std.HashMap Address Nat := Std.HashMap.empty
  deriving Repr

inductive TransactionType
  | external -- Inbox/Outbox related
  | internal
  deriving Repr, BEq, DecidableEq

structure PassTransaction where
  txType : TransactionType
  sender : Address
  recipient : Address
  asset : Asset
  amount : Nat
  deriving Repr

def Asset.mkEmpty (assetId : String) : Asset :=
  if assetId == "ether" then
    { id := assetId, assetType := AssetType.Ether, balanceMap := Std.HashMap.empty }
  else if assetId == "null" then
    { id := assetId, assetType := AssetType.Undefined, balanceMap := Std.HashMap.empty }
  else
    { id := assetId, assetType := AssetType.Token, balanceMap := Std.HashMap.empty }

def Asset.getBalance (self : Asset) (address : Address) : Nat :=
  match self.balanceMap.get? address with
  | some bal => bal
  | none => 0

def Asset.getTotalBalance (self : Asset) : Nat :=
  self.balanceMap.fold (fun sum _ bal => sum + bal) 0

def Asset.updateBalance (asset : Asset) (address : Address) (amount : Nat) : Asset :=
  let newBalanceMap := asset.balanceMap.insert address amount
  { asset with balanceMap := newBalanceMap }


def Asset.hasBalance (asset : Asset) (address : Address) (amount : Nat) : Bool :=
  asset.getBalance address >= amount

-- Creates a new asset from a transaction
def Asset.getAssetFromTx (tx : Transaction) (worldState : WorldState) : Asset :=
  if isEOA worldState tx.to_address then
    { id := "ether",
      assetType := AssetType.Ether,
      balanceMap := Std.HashMap.empty.insert tx.from_address tx.value }
  else
  -- If it's a token transfer, then we can use the token address as the asset id. Value is in the second element of the data field
  let value := match tx.data with
    | _::v::_ => match v.toNat? with
      | some value => value
      | none => 0
    | _ => 0

  let balanceMap := Std.HashMap.empty.insert tx.from_address value
  { id := tx.to_address, assetType := AssetType.Token, balanceMap }
