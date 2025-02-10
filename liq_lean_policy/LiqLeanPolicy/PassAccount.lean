import LiqLeanPolicy.Asset
import LiqLeanPolicy.EVMState
structure SubAccount where
  /-- Unique identifier for the subaccount -/
  id : String
  owner : Address
  deriving Repr, BEq, DecidableEq

structure Inbox extends SubAccount where
  claimMap : Std.HashMap Address (String × Nat) := Std.HashMap.empty -- Sender, AssetID, ClaimAmount
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
  let senderAssets := self.inbox.claimMap.get? tx.from_address
  let newAmount := match senderAssets with
    | some (id, amount) => if id = asset.id
                          then amount + value  -- Increment existing claim
                          else value          -- Different asset, use new value
    | none => value                          -- No existing claim, use new value
  let newClaimMap := self.inbox.claimMap.insert tx.from_address (asset.id, newAmount)
  let newInbox := { self.inbox with claimMap := newClaimMap }
  { self with inbox := newInbox }


-- Process a claimMap claim from the inbox
def PassAccount.processClaim (self : PassAccount) (claimAsset : Asset) (claimer : Address) :=
  self






-- def PassAccount.transferAsset (self : PassAccount) (assetId : String) (sender : SubAccount) (recipient : SubAccount) (amount : Nat) : PassAccount :=
--   -- TODO: Implement this
--   self

-- def PassAccount.sendAsset (self : PassAccount) (assetId : String) (sender : SubAccount) (recipient : SubAccount) (amount : Nat) : PassAccount :=
--   -- TODO: Implement this
--   self
