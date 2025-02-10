import Std.Data.HashMap

/- Simplified EVM state model-/
/- Represents an Ethereum address as a 160-bit number -/
def Address := String
deriving Repr, BEq, DecidableEq, Hashable

/- Represents Wei (smallest Ethereum unit) amount -/
def Wei := Nat
deriving Repr, BEq, DecidableEq

/- Basic transaction structure -/
structure Transaction where
  nonce: Nat
  from_address: Address
  to_address: Address
  value: Wei
  data: List String
  gasPrice: Wei
  gasLimit: Nat
  deriving Repr, BEq, DecidableEq

/- Account state of EVM -/
/- EOAs and contracts are both accounts -/
structure Account where
  balance: Wei
  nonce: Nat
  code: List Nat
  storage: Std.HashMap Nat Nat := Std.HashMap.empty

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
def WorldState : Std.HashMap Address Account := Std.HashMap.empty

/- Blockchain as a list of blocks -/
def Blockchain := List Block

def isEOA (address : Address) : Bool :=
  -- If address in WorldState and account.code is not empty, then not EOA
  match WorldState.get? address with
  | some account => account.code.isEmpty -- If code is not empty, then smart contract
  | none => true -- Assume all other addresses are EOAs

-- Convert a Float to a Nat
def toNat (x : Float) : Nat :=
    Float.toUInt64 x |>.toNat

def parseEther (value : Float) : Wei :=
  let weiValue := (value * 10^18)
  toNat weiValue

def parseGwei (value : Float) : Wei :=
  let weiValue := (value * 10^9)
  toNat weiValue
