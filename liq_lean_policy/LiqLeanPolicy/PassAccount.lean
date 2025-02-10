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
  deriving Repr

def PassAccount.getAsset (self : PassAccount) (assetId : String)  : Option Asset :=
  self.assets.find? (fun asset => asset.id = assetId)

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
  outbox := { id := "outbox", owner := eoa, txQueue := [], nonce := 0 }
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


-- Basic access control policy for Internal Transactions.
def PassAccount.checkAllow (self : PassAccount) (assetId : String) (recipient : Address) (amount : Nat) : Bool :=
  true -- Allow everything for everyone

def PassAccount.outboxSubmit (self : PassAccount) : (PassAccount × List Transaction) :=
  -- TODO: Loop for each pending transaction in outbox. Dequeue, sign, and create new Transaction
  let resultTxList : List Transaction := []
  for tx in self.outbox.txQueue do
    match tx.asset.assetType with
    | AssetType.Ether =>
    -- Create new TX
      let newTx : Transaction := {
      nonce := self.outbox.nonce,
      from_address := self.eoaAccount,
      to_address := tx.recipient,
      value := tx.amount,
      data := [],
      gasPrice := getGasPrice,
      gasLimit := getGasLimit,
      }
     }
    -- Pop from outbox and append to resultTxList
    | _ => none -- TODO: Handle non-ether assets
  (self, [])


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
        let newOutbox := { self.outbox with txQueue := tx :: self.outbox.txQueue }
        let newSelf := { self with outbox := newOutbox }
        (newSelf.setAsset newAsset, true)
      else
        -- Process internal transaction by updating recipient asset balance
        let newAsset := Asset.updateBalance newAsset tx.recipient (currAsset.getBalance tx.recipient + tx.amount)
        (self.setAsset newAsset, true)
  else
    (self, false)
