import LiqLeanPolicy.EVMState
/- Asset representation for access control -/
structure Asset where
  id: Nat
  assetType: String    -- e.g., "ETH", "ERC20", "NFT"
  amount: Option Wei   -- None for NFTs, Some for fungible tokens
  deriving Repr, BEq

/- Time-bound access rights -/
structure AccessRight where
  player: Address
  asset: Asset
  startTime: Nat
  endTime: Nat
  deriving Repr, BEq

/- Sub-policy as defined in paper Section 4.1 -/
structure SubPolicy where
  id: Nat
  parent: Option Nat        -- None for root policy
  accessRights: List AccessRight
  spentBalance: Finmap (λ _ : Asset ↦ Wei)  -- Tracks spent amounts for fungible assets

/- Liquefaction account extends regular Account -/
structure LiquefactionAccount extends Account where
  -- TEE-related fields
  teeAttestation: List Nat      -- Attestation from trusted execution environment
  encumberedPrivateKey: List Nat -- Encrypted private key only accessible by TEE

  -- Access control fields
  accessManager: Address        -- Controls policy transitions
  policies: List SubPolicy      -- Delegation tree of sub-policies
  currentNonce: Nat            -- For preventing pre-signing attacks

  -- State tracking
  committedTransactions: List Transaction  -- For preventing double-spending

/- Policy validity checking as per Section 3.2 -/
def isValidPolicy (account: LiquefactionAccount) : Prop :=
  -- Asset-time segmentation: single owner per asset at any time within same account
  ∀ (p1 p2: SubPolicy) (r1 r2: AccessRight),
    p1 ∈ account.policies →
    p2 ∈ account.policies →
    r1 ∈ p1.accessRights →
    r2 ∈ p2.accessRights →
    r1.asset = r2.asset →
    (r1.endTime < r2.startTime ∨ r2.endTime < r1.startTime)

/- Check if player has access to asset at given time -/
def hasAccess (account: LiquefactionAccount) (player: Address)
    (asset: Asset) (time: Nat) : Bool :=
  account.policies.any fun policy =>
    policy.accessRights.any fun right =>
      right.player == player &&
      right.asset == asset &&
      right.startTime <= time &&
      time <= right.endTime
