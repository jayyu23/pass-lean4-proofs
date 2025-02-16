import LiqLeanPolicy.Asset
import LiqLeanPolicy.EVMState
import Std.Data.HashSet



structure SubAccount where
  /-- Unique identifier for the subaccount -/
  id : String
  owner : Address
  deriving Repr, BEq, DecidableEq

structure Inbox extends SubAccount where
  claimMap : Std.HashMap Address (Std.HashMap String Nat) := Std.HashMap.empty -- Sender, AssetID, ClaimAmount
  deriving Repr

structure Outbox extends SubAccount where
  txQueue : List PassTransaction
  nonce : Nat
  deriving Repr

structure PassAccount where
  /-- Unique identifier for the pass account -/
  id : String
  subaccounts : List SubAccount
  eoaAccount : Address -- The affiliated EOA account address
  assets : List Asset
  inbox : Inbox
  outbox : Outbox
  messageAsset : GeneralSignableMessage
  deriving Repr


def PassAccount.getAsset (self : PassAccount) (assetId : String)  : Option Asset :=
  self.assets.find? (fun asset => asset.id = assetId)

-- Wrapper for getAsset that returns a null asset if the asset is not found
def PassAccount.getAssetOrNull (self : PassAccount) (assetId : String) : Asset :=
  match self.getAsset assetId with
  | some asset => asset
  | none => Asset.mkEmpty "null"

def PassAccount.setAsset (self : PassAccount) (asset : Asset) : PassAccount :=
  match self.assets.find? (fun a => a.id = asset.id) with
  | some _ => { self with assets := self.assets.map (fun a => if a.id = asset.id then asset else a) }
  | none => { self with assets := asset :: self.assets }

def PassAccount.removeAsset (self : PassAccount) (assetId : String) : PassAccount :=
  { self with assets := self.assets.filter (fun a => a.id != assetId) }

def PassAccount.mkEmpty (eoa : Address) : PassAccount := {
  id := "",
  subaccounts := [],
  eoaAccount := eoa,
  assets := [],
  inbox := { id := "inbox", owner := eoa, claimMap := Std.HashMap.empty },
  outbox := { id := "outbox", owner := eoa, txQueue := [], nonce := 0 },
  messageAsset := GeneralSignableMessage.mkEmpty eoa
}

-- Processes a standard EVM transaction
def PassAccount.processExternalTx (self : PassAccount) (tx : Transaction) (worldState : WorldState) : PassAccount :=
  -- Get Asset from EVM Transaction
  let asset := Asset.getAssetFromTx tx worldState
  let value := asset.getBalance tx.from_address

  -- Update inbox
  let senderAssetMap := match self.inbox.claimMap.get? tx.from_address with
    | none => Std.HashMap.empty  -- Empty sender map
    | some map => map           -- Existing sender map

  let newAmount := match senderAssetMap.get? asset.id with
    | none => value             -- New asset claim
    | some amount => amount + value  -- Add to existing claim

  let newSenderAssetMap := senderAssetMap.insert asset.id newAmount
  let newClaimMap := self.inbox.claimMap.insert tx.from_address newSenderAssetMap
  { self with inbox := { self.inbox with claimMap := newClaimMap } }

-- Process a claimMap claim from the inbox
def PassAccount.processClaim (self : PassAccount) (claimAssetId : String) (claimer : Address) :=
  -- Get claim amount
  let claimAsset := match self.getAsset claimAssetId with
    | none => Asset.mkEmpty claimAssetId
    | some asset => asset

  let claimerAssetMap := match self.inbox.claimMap.get? claimer with
    | none => Std.HashMap.empty
    | some map => map

  let claimAmount := match claimerAssetMap.get? claimAssetId with
    | none => 0
    | some amount => amount

  let newBalance := claimAsset.getBalance claimer + claimAmount
  let newAsset := Asset.updateBalance claimAsset claimer newBalance

  -- Update inbox: remove the claimed asset from sender's asset map
  let newClaimerAssetMap := claimerAssetMap.erase claimAsset.id
  let newClaimMap := self.inbox.claimMap.insert claimer newClaimerAssetMap
  let newSelf := self.setAsset newAsset
  { newSelf with inbox := { newSelf.inbox with claimMap := newClaimMap } }



def PassAccount.checkBalance (self : PassAccount) (assetId : String) (requestor : Address) (amount : Nat) : Bool :=
  let asset := self.getAsset assetId
  match asset with
  | none => false
  | some asset => asset.getBalance requestor >= amount


-- Basic access control policy for Internal Transactions. This can be extended to support any decidable function.
def PassAccount.checkAllow (self : PassAccount) (assetId : String) (recipient : Address) (amount : Nat) : Bool :=
  true -- Allow everything for everyone

-- Trivial demonstration that checkAllow is decidable
instance (self : PassAccount) (assetId : String) (recipient : Address) (amount : Nat) :
  Decidable (PassAccount.checkAllow self assetId recipient amount) :=
  isTrue rfl

def PassAccount.outboxSubmit (self : PassAccount) : (PassAccount × List Transaction) :=

  let initState := (self, [])

  let (acc, resultTxList) := self.outbox.txQueue.foldl (fun (state : PassAccount × List Transaction) tx =>
    let (acc, resultTxList) := state
    match tx.asset.assetType with
    | AssetType.Ether =>
      let newTx : Transaction := {
        nonce := acc.outbox.nonce,
        from_address := acc.eoaAccount,
        to_address := tx.recipient,
        value := tx.amount,
        data := [],
        gasPrice := getGasPrice,
        gasLimit := getGasLimit
      }
      let newAcc := { acc with outbox := { acc.outbox with nonce := acc.outbox.nonce + 1 } }
      (newAcc, resultTxList.append [newTx])
    | AssetType.Token =>
      let contractAddress := tx.asset.id
      let newTx : Transaction := {
        nonce := acc.outbox.nonce,
        from_address := acc.eoaAccount,
        to_address := contractAddress,
        value := parseEther 0,
        data := [tx.recipient, toString tx.amount],
        gasPrice := getGasPrice,
        gasLimit := getGasLimit
      }
      let newAcc := { acc with outbox := { acc.outbox with nonce := acc.outbox.nonce + 1 } }
      (newAcc, resultTxList.append [newTx])
    | _ => (acc, resultTxList)
  ) initState
  (acc, resultTxList)

-- Process Internal Transaction. Returns (PassAccount, Status : Bool)
def PassAccount.processInternalTx (self : PassAccount) (tx : PassTransaction) : (PassAccount × Bool) :=
  if self.checkBalance tx.asset.id tx.sender tx.amount && self.checkAllow tx.asset.id tx.recipient tx.amount then
    let currAsset := self.getAsset tx.asset.id
    match currAsset with
    | none => (self, false)
    | some currAsset =>
      let oldBalance := currAsset.getBalance tx.sender
      let newAsset := Asset.updateBalance currAsset tx.sender (oldBalance - tx.amount)
      if tx.txType = TransactionType.external then
        -- Send to the outbox
        let newOutbox := { self.outbox with txQueue := self.outbox.txQueue ++ [tx] }
        let newSelf := { self with outbox := newOutbox }
        (newSelf.setAsset newAsset, true)
      else
        -- Process internal transaction by updating recipient asset balance
        let newAsset := Asset.updateBalance newAsset tx.recipient (currAsset.getBalance tx.recipient + tx.amount)
        (self.setAsset newAsset, true)
  else
    (self, false)
