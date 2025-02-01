import LiqLeanPolicy.EVMState
import LiqLeanPolicy.Asset

structure SubAccount where
  /-- Unique identifier for the subaccount -/
  id : String
  deriving Inhabited, Repr

structure PassAccount where
  /-- Unique identifier for the pass account -/
  id : String
  subaccounts : List SubAccount
  nonce : Nat
  deriving Inhabited, Repr

def isInbox (sa : SubAccount) : Bool := sa.id = "inbox"
def isOutbox (sa : SubAccount) : Bool := sa.id = "outbox"

-- Only Outbox can send
def send (pa : PassAccount) (sa : SubAccount) (asset : Asset) : (PassAccount × Bool) :=
  -- check can send
  if ( isOutbox sa == true
  -- check valid provenance
  ∧ hasValidProvenance asset)
  then
    ({pa with nonce := pa.nonce + 1}, True)
  else
    (pa, False)
