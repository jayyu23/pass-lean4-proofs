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
  txQueue : List Transaction
  nonce : Nat
  deriving Repr, BEq, DecidableEq

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
  { self with assets := self.assets.map (fun a => if a.id = asset.id then asset else a) }

def PassAccount.removeAsset (self : PassAccount) (assetId : String) : PassAccount :=
  { self with assets := self.assets.filter (fun a => a.id != assetId) }

-- def PassAccount.claimAsset (self : PassAccount) (assetId : String) (requester : SubAccount) : PassAccount :=
--   match self.inbox.claimMap.find? (fun (sender, id, _) => id = assetId && requester.owner = sender) with
--   | none => self  -- No claim found in inbox, return unchanged
--   | some (sender, _, claimAmount) =>
--     match self.getAsset assetId with
--     | some asset =>
--       let newBalance := asset.getBalance requester.owner + claimAmount
--       -- Update Asset
--       let newAsset := Asset.updateBalance asset requester.owner newBalance
--       let updatedAssets := self.assets.map (fun a => if a.id = assetId then newAsset else a)
--       -- Update Inbox
--       let newInbox := { self.inbox with claimMap := self.inbox.claimMap.filter (fun (s, id, _) => !(s = sender && id = assetId)) }
--       { self with assets := updatedAssets, inbox := newInbox }  -- Use updatedAssets
--     | none => self  -- Asset not found, return unchanged

-- Processes a standard EVM transaction
def PassAccount.processExternalTx (self : PassAccount) (tx : Transaction) : PassAccount :=
  -- Get Asset from EVM Transaction
  let asset := Asset.getAssetFromTx tx
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
def PassAccount.processClaim (self : PassAccount) (claimAsset : Asset) (claimer : Address) :=
  -- Get claim amount
  let claimerAssetMap := match self.inbox.claimMap.get? claimer with
    | none => Std.HashMap.empty
    | some map => map

  let claimAmount := match claimerAssetMap.get? claimAsset.id with
    | none => 0
    | some amount => amount

  let newBalance := claimAsset.getBalance claimer + claimAmount
  let newAsset := Asset.updateBalance claimAsset claimer newBalance

  -- Update inbox: remove the claimed asset from sender's asset map
  let newClaimerAssetMap := claimerAssetMap.erase claimAsset.id
  let newClaimMap := self.inbox.claimMap.insert claimer newClaimerAssetMap
  let newSelf := self.setAsset newAsset
  { newSelf with inbox := { newSelf.inbox with claimMap := newClaimMap } }
