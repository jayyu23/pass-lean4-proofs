import LiqLeanPolicy.EVMState

structure SubAccount where
  /-- Unique identifier for the subaccount -/
  id : String
  /-- List of asset instances with provenance tracking -/
  assets : List AssetInstance
  /-- Sequence number for operation ordering -/
  localNonce : Nat
  /-- Permissions for the subaccount -/
  permissions : PermissionSet
  deriving Inhabited, Repr




/-- Complementary structures --/
structure AssetInstance where
  asset : Asset
  provenanceChain : List ProvenanceStep
  locked : Bool := false

inductive ProvenanceStep where
  | externalIn (txHash : String)  /- Incoming blockchain transaction -/
  | internalTransfer (fromSubaccount : String)  /- Internal transfer -/
  | gsmRequest (domain : String)  /- GSM signature request -/

structure PermissionSet where
  /-- Can initiate withdrawals to outbox -/
  canWithdraw : Bool := false
  /-- Can receive internal transfers -/
  canReceive : Bool := true
  /-- Maximum transfer amount per operation -/
  transferLimit : Option Nat := none


def isInbox (sa : SubAccount) : Bool := sa.id = "inbox"
def isOutbox (sa : SubAccount) : Bool := sa.id = "outbox"

/-- Inbox specific rules -/
theorem inbox_permissions (sa : SubAccount) : isInbox sa → sa.permissions.canWithdraw = false := by
  intro h
  simp [isInbox] at h
  subst h
  simp [PermissionSet]

/-- Outbox specific rules -/
def outboxConstraints (sa : SubAccount) : Prop :=
  isOutbox sa →
  sa.permissions.canReceive = false ∧
  sa.assets.all (fun ai => ai.asset.assetType = AssetType.GSM)
