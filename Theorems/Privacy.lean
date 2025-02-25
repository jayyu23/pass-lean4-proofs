import Std

/- We adopt a simplified version of the PassAccount model as defined in PassWalletModels -/

/-- Minimal transaction structure. --/
structure Transaction where
  nonce     : Nat
  from_addr : String
  to_addr   : String
  value     : Nat
  data      : List String
  gasPrice  : Nat
  gasLimit  : Nat
deriving Repr, BEq

/-- External actions visible on-chain. --/
inductive ExternalAction where
  | outboxTx (tx : Transaction) : ExternalAction
  | inboundTx (tx : Transaction) : ExternalAction
deriving Repr, BEq

/-- Minimal outbox structure. --/
structure Outbox where
  txQueue : List Transaction
  nonce   : Nat
deriving Repr, BEq

/-- Minimal asset structure (tracking only an id and its total balance). --/
structure Asset where
  id           : String
  totalBalance : Nat
deriving Repr, BEq

namespace Asset
  def getTotalBalance (a : Asset) : Nat := a.totalBalance
end Asset

/-- Minimal PassAccount with an EOA, an outbox, and a list of assets. --/
structure PassAccount where
  eoaAccount : String
  outbox     : Outbox
  assets     : List Asset
deriving Repr, BEq

namespace PassAccount
  /-- Returns the asset with the given id or a “null asset” if not found. --/
  def getAssetOrNull (p : PassAccount) (assetId : String) : Asset :=
    match p.assets.find? (fun a => a.id = assetId) with
    | some a => a
    | none   => { id := "null", totalBalance := 0 }
end PassAccount

/-- Extract the externally visible trace from a PassAccount.
    Here we assume that the only externally visible events are outbox transactions. --/
def externalTrace (p : PassAccount) : List ExternalAction :=
  p.outbox.txQueue.map ExternalAction.outboxTx

/-- Two PassAccount states differ only by internal transfers if:
  - They share the same EOA,
  - They have the same outbox transaction queue and nonce, and
  - The total balance for each asset (as seen externally) is equal.
--/
def differByInternalTransfers (s₁ s₂ : PassAccount) : Prop :=
  s₁.eoaAccount = s₂.eoaAccount ∧
  s₁.outbox.txQueue = s₂.outbox.txQueue ∧
  s₁.outbox.nonce   = s₂.outbox.nonce ∧
  (∀ assetId, (PassAccount.getAssetOrNull s₁ assetId).getTotalBalance =
               (PassAccount.getAssetOrNull s₂ assetId).getTotalBalance)

/--
  Theorem: If two PassAccount states differ only by internal transfers, then their
  externally visible traces are identical.

  Proof: Since `externalTrace` is defined solely as a mapping over the outbox transaction
  queue, equality of the txQueue in both states immediately yields equality of the traces.
--/
theorem internalTransfersPreserveTrace (s₁ s₂ : PassAccount)
  (h : differByInternalTransfers s₁ s₂) :
  externalTrace s₁ = externalTrace s₂ :=
by
  obtain ⟨h_eoa, h_tx, h_nonce, h_assets⟩ := h
  dsimp [externalTrace]
  rw [h_tx]
