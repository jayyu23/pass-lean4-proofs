import LiqLeanPolicy.PassAccount

def eoaAddress : Address := "0x1111"
def addressB : Address := "0x2222"
def addressC : Address := "0x3333"
def addressD : Address := "0x4444"
def usdc_contract : Address := "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
def passAccount1 : PassAccount := PassAccount.mkEmpty eoaAddress

-- World state with USDC contract deployed
def worldState : WorldState :=
  let world := Std.HashMap.empty
  setContract world usdc_contract

-- Make a transaction from addressB to addressA
def tx1 : Transaction := {
  nonce := 0,
  from_address := addressB,
  to_address := eoaAddress,
  value := parseEther 1.0,
  data := [],
  gasPrice := parseGwei 30,
  gasLimit := 21000
}

def tx2 : Transaction := {
  nonce := 1,
  from_address := addressB,
  to_address := eoaAddress,
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
  data := [eoaAddress, "1000"], -- 1000 USDC to addressA
  gasPrice := parseGwei 30,
  gasLimit := 21000
}

def tx4 : Transaction := {
    nonce := 3,
    from_address := addressC,
    to_address := usdc_contract,
    value := parseEther 1.0,
    data := [eoaAddress, "500"],
    gasPrice := parseGwei 30,
    gasLimit := 21000
  }

def tx5 : Transaction := {
    nonce := 4,
    from_address := addressC,
    to_address := usdc_contract,
    value := parseEther 1.0,
    data := [eoaAddress, "3500"],
    gasPrice := parseGwei 30,
    gasLimit := 21000
  }

def test1 : PassAccount :=
  let pa1 := passAccount1.processExternalTx tx1 worldState
  pa1.processExternalTx tx2 worldState

def test2 : PassAccount :=
  let pa1 := passAccount1.processExternalTx tx1 worldState
  let pa2 := pa1.processExternalTx tx3 worldState
  let pa3 := pa2.processExternalTx tx4 worldState
  pa3.processExternalTx tx5 worldState

-- Test claim
def test3 : PassAccount :=
  let pa := test2
  let pa1 := pa.processClaim "ether" addressB
  let pa2 := pa1.processClaim usdc_contract addressC
  pa2.processClaim usdc_contract addressB

def test4 : PassAccount × Bool :=
  let pa := test3
  let pa2 := pa.processClaim "ether" addressB
  let asset := pa2.getAssetOrNull "ether"

  let usdcAsset := pa2.getAssetOrNull usdc_contract
  -- Ether transfer from addressB to addressC
  let internalTx1 : PassTransaction := {
    txType := TransactionType.internal,
    sender := addressB,
    recipient := addressC,
    amount := parseEther 0.5,
    asset := asset
  }
  let internalTx2 : PassTransaction := {
    txType := TransactionType.internal,
    sender := addressC,
    recipient := addressB,
    amount := 4000,
    asset := usdcAsset
  }

  let (pa3, _) := pa2.processInternalTx internalTx1
  pa3.processInternalTx internalTx2

-- Test transaction to outbox
def test5 : (PassAccount × Bool) :=
  let pa := test4.1
  let etherAsset := pa.getAssetOrNull "ether"
  let usdcAsset := pa.getAssetOrNull usdc_contract

  let outboxTx : PassTransaction := {
    txType := TransactionType.external,
    sender := addressB,
    recipient := addressD,
    amount := parseEther 0.5,
    asset := etherAsset
  }

  let outboxTx2 : PassTransaction := {
    txType := TransactionType.external,
    sender := addressB,
    recipient := addressD,
    amount := 100,
    asset := usdcAsset
  }
  let outboxTx3 : PassTransaction := {
    txType := TransactionType.external,
    sender := addressB,
    recipient := addressC,
    amount := 1000,
    asset := usdcAsset
  }
  let (pa1, _) := pa.processInternalTx outboxTx
  let (pa2, _) := pa1.processInternalTx outboxTx2
  pa2.processInternalTx outboxTx3

-- Test outbox emit Transaction
def test6 : (PassAccount × List Transaction) :=
  let pa := test5.1
  pa.outboxSubmit

#eval test3.assets
#eval test4.1.assets
#eval test5.1.outbox.txQueue
#eval test6.2
