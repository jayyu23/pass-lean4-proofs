import PassWalletModels.EVMState

inductive AssetType
  | Null
  | Ether -- ETH is a special asset case
  | Token -- Non-ETH Token contract
  | GSM -- General Signable Message
  deriving Repr, BEq, DecidableEq

structure Asset where
  id : String
  assetType : AssetType
  balanceMap : Std.HashMap Address Nat := Std.HashMap.empty
  totalBalance : Nat := 0
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
    { id := assetId, assetType := AssetType.Null, balanceMap := Std.HashMap.empty }
  else
    { id := assetId, assetType := AssetType.Token, balanceMap := Std.HashMap.empty }

instance : Inhabited Asset where
  default := Asset.mkEmpty "null"

def Asset.isEqual (self : Asset) (other : Asset) : Bool :=
  self.id = other.id &&
  self.assetType = other.assetType &&
  self.balanceMap.toList == other.balanceMap.toList

instance : BEq Asset where
  beq a b := Asset.isEqual a b

def Asset.getBalance (self : Asset) (address : Address) : Nat :=
  match self.balanceMap.get? address with
  | some bal => bal
  | none => 0

def Asset.isNull (self : Asset) : Bool :=
  self.id == "null" || self.assetType == AssetType.Null

def Asset.isGSM (self : Asset) : Bool :=
  self.id == "message" || self.assetType == AssetType.GSM

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

-- General Signable Message
structure GeneralSignableMessage extends Asset where
  -- Map of addresses to a list of GSMs
  domainAddressMap : Std.HashMap String Address := Std.HashMap.empty
  defaultSigner : Address
  deriving Repr

def GeneralSignableMessage.mkEmpty (defaultSigner : Address) : GeneralSignableMessage :=
  { id := "message", assetType := AssetType.GSM, balanceMap := Std.HashMap.empty, domainAddressMap := Std.HashMap.empty, defaultSigner }

def GeneralSignableMessage.hasDomain (self : GeneralSignableMessage) (domain : String) : Bool :=
  self.domainAddressMap.contains domain

def GeneralSignableMessage.getSigner (self : GeneralSignableMessage) (domain : String) : Address :=
  match self.domainAddressMap.get? domain with
  | some signer => signer
  | none => self.defaultSigner

def GeneralSignableMessage.getDomains (self : GeneralSignableMessage) (address : Address) : List String :=
  -- Find all domains where address is the signer
  self.domainAddressMap.fold (fun acc domain signer =>
    if signer == address then domain :: acc else acc) []

def GeneralSignableMessage.setDomain (self : GeneralSignableMessage) (domain : String) (address : Address) : GeneralSignableMessage :=
  let newDomainMap := self.domainAddressMap.insert domain address
  { self with domainAddressMap := newDomainMap }
