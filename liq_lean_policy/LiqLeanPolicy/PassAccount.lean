import LiqLeanPolicy.Asset

structure SubAccount where
  /-- Unique identifier for the subaccount -/
  id : String
  owner : Address
  deriving Repr, BEq, DecidableEq

structure Inbox extends SubAccount where
  claimMap : List (Address × String × Nat) -- Sender, AssetID, ClaimAmount
  deriving Repr, BEq, DecidableEq

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
  deriving Repr, BEq, DecidableEq

def PassAccount.getAsset (self : PassAccount) (assetId : String)  : Option Asset :=
  self.assets.find? (fun asset => asset.id = assetId)

def PassAccount.claimAsset (self : PassAccount) (assetId : String) (requester : SubAccount) : PassAccount :=
  -- Check if there's a matching claim in the inbox
  match self.inbox.claimMap.find? (fun (sender, id, amount) => id = assetId && requester.owner = sender) with
  | none => self  -- No claim found in inbox, return unchanged
  | some (sender, _, claimAmount) =>
    match self.getAsset assetId with
    | some asset =>
      -- Update asset map by adding the claim amount
      let newBalance := asset.getBalance requester.owner + claimAmount
      let newAsset := { asset with balanceMap := (requester.owner, newBalance) :: asset.balanceMap }
      -- Remove the claim from inbox after processing
      let newInbox := { self.inbox with claimMap := self.inbox.claimMap.filter (fun (s, id, _) => !(s = sender && id = assetId)) }
      -- Update the assets list by replacing the old asset with the new one
      let newAssets := self.assets.map (fun a => if a.id = assetId then newAsset else a)
      { self with assets := newAssets, inbox := newInbox }
    | none => self
