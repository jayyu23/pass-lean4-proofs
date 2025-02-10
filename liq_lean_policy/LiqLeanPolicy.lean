-- This module serves as the root of the `LiqLeanPolicy` library.
import LiqLeanPolicy.PassAccount

def addressA : Address := "0x1234"
def addressB : Address := "0x1235"
def passAccount1 : PassAccount := PassAccount.mkEmpty addressA

#check passAccount1

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

def test1 : PassAccount :=
  let pa1 := passAccount1.processExternalTx tx1
  -- pa1.processExternalTx tx2
  pa1

#eval test1
