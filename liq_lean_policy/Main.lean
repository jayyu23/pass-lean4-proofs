import LiqLeanPolicy.PassAccount

def eoaAddress : Address := "0x1111"
def addressB : Address := "0x2222"
def addressC : Address := "0x3333"
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
  let asset := pa2.getAsset "ether"
  let asset := match asset with
    | some asset => asset
    | none => Asset.mkEmpty "null"

  let usdcAsset := pa2.getAsset usdc_contract
  let usdcAsset := match usdcAsset with
    | some asset => asset
    | none => Asset.mkEmpty "null"

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

  -- let (pa1, _) := pa2.processInternalTx internalTx1
  -- pa1.processInternalTx internalTx2
  let (pa3, _) := pa2.processInternalTx internalTx1
  pa3.processInternalTx internalTx2

#eval test3.assets
#eval test4.1.assets

-- -- Test internalTx
-- def test4 : PassAccount :=
--   let pa := test2

-- -- Test processClaim with multiple assets and claimers
-- def testProcessClaim1 : PassAccount :=
--   let pa1 := passAccount1.processExternalTx tx3 worldState  -- USDC transaction
--   let pa2 := pa1.processExternalTx tx1 worldState          -- ETH transaction
--   pa2.processClaim "usdc" addressB                         -- Claim USDC

-- def testProcessClaim2 : PassAccount :=
--   let pa1 := test2  -- Account with multiple transactions
--   let pa2 := pa1.processClaim "usdc" addressC             -- Claim USDC
--   pa2.processClaim "ether" addressB                       -- Claim ETH

-- -- Test internal transactions


-- def internalTx2 : PassTransaction := {
--   txType := TransactionType.external,
--   sender := addressA,
--   recipient := addressC,
--   amount := parseEther 0.3,
--   asset := Asset.mkEmpty "ether"
-- }

-- def testInternalTx1 : (PassAccount × Bool) :=
--   let pa1 := test3  -- Account with claimed assets
--   pa1.processInternalTx internalTx1

-- def testInternalTx2 : (PassAccount × Bool) :=
--   let pa1 := test3
--   let (pa2, _) := pa1.processInternalTx internalTx1
--   pa2.processInternalTx internalTx2

-- -- Test outbox submission
-- def testOutboxSubmit1 : (PassAccount × List Transaction) :=
--   let pa1 := test3
--   let (pa2, _) := pa1.processInternalTx internalTx2  -- Add external tx to outbox
--   pa2.outboxSubmit

-- -- Test combined flow
-- def testCombinedFlow : (PassAccount × List Transaction) :=
--   let pa1 := test2
--   let pa2 := pa1.processClaim "ether" addressB
--   let (pa3, _) := pa2.processInternalTx internalTx2
--   pa3.outboxSubmit

-- -- #eval testProcessClaim1.assets
-- -- #eval testProcessClaim2.inbox
-- #eval (testInternalTx1.2) -- Check success status
-- -- #eval testInternalTx2.0.outbox  -- Check outbox queue
-- -- #eval testOutboxSubmit1.1  -- Check generated transactions
-- -- #eval testCombinedFlow.1

-- -- #eval test2.inbox
-- -- #eval test3
-- #eval test3.assets
