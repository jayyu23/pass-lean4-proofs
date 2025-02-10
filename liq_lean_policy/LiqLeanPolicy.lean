-- This module serves as the root of the `LiqLeanPolicy` library.
import LiqLeanPolicy.PassAccount

def addressA : Address := "0x1111"
def addressB : Address := "0x2222"
def addressC : Address := "0x3333"
def usdc_contract : Address := "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
def passAccount1 : PassAccount := PassAccount.mkEmpty addressA

-- World state with USDC contract deployed
def worldState : WorldState :=
  let world := Std.HashMap.empty
  setContract world usdc_contract

#eval worldState
-- #eval isEOA worldState addressA
-- #eval isEOA worldState usdc_contract

-- Make a transaction from addressB to addressA
def tx1 : Transaction := {
  nonce := 0,
  from_address := addressB,
  to_address := addressA,
  value := parseEther 1.0,
  data := [],
  gasPrice := parseGwei 30,
  gasLimit := 21000
}

def tx2 : Transaction := {
  nonce := 1,
  from_address := addressB,
  to_address := addressA,
  value := parseEther 1.0,
  data := [],
  gasPrice := parseGwei 30,
  gasLimit := 21000
}

def tx3 : Transaction := {
  nonce := 2,
  from_address := addressB,
  to_address := usdc_contract,
  value := parseEther 1.0,
  data := ["1000", addressA], -- 1000 USDC to addressA
  gasPrice := parseGwei 30,
  gasLimit := 21000
}

def tx4 : Transaction := {
    nonce := 3,
    from_address := addressC,
    to_address := usdc_contract,
    value := parseEther 1.0,
    data := [addressA, "500"],
    gasPrice := parseGwei 30,
    gasLimit := 21000
  }

def tx5 : Transaction := {
    nonce := 4,
    from_address := addressC,
    to_address := usdc_contract,
    value := parseEther 1.0,
    data := [addressA, "3500"],
    gasPrice := parseGwei 30,
    gasLimit := 21000
  }

-- #eval getAssetFromTx1

-- #eval tx1
def test1 : PassAccount :=
  let pa1 := passAccount1.processExternalTx tx1 worldState
  pa1.processExternalTx tx2 worldState

def test2 : PassAccount :=
  let pa1 := passAccount1.processExternalTx tx1 worldState
  let pa2 := pa1.processExternalTx tx3 worldState
  let pa3 := pa2.processExternalTx tx4 worldState
  pa3.processExternalTx tx5 worldState

#eval test2.inbox.claimMap
