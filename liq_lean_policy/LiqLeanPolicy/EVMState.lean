import Mathlib.Data.Finmap


/- Simplified EVM state model-/
/- Represents an Ethereum address as a 160-bit number -/
def Address := Nat
deriving Repr, BEq, DecidableEq

/- Represents Wei (smallest Ethereum unit) amount -/
def Wei := Nat
deriving Repr, BEq, DecidableEq

/- Basic transaction structure -/
structure Transaction where
  nonce: Nat
  from_address: Address
  to_address: Address
  value: Wei
  data: List Nat
  gasPrice: Wei
  gasLimit: Nat
  deriving Repr, BEq, DecidableEq

/- Account state of EVM -/
/- EOAs and contracts are both accounts -/
structure Account where
  balance: Wei
  nonce: Nat
  code: List Nat
  storage: Finmap (λ _ : Nat ↦ Nat)

/- Block header -/
structure BlockHeader where
  parentHash: Nat
  timestamp: Nat
  number: Nat
  stateRoot: Nat
  transactionsRoot: Nat
  receiptsRoot: Nat
  deriving Repr, BEq, DecidableEq

/- Block structure -/
structure Block where
  header: BlockHeader
  transactions: List Transaction
  deriving Repr, BEq, DecidableEq

/- World state as a mapping from addresses to accounts -/
def WorldState := Finmap (λ _ : Address ↦ Account)

/- Blockchain as a list of blocks -/
def Blockchain := List Block
