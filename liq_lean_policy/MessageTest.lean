import LiqLeanPolicy.PassAccount
import LiqLeanPolicy.Asset


-- TODO: Test out GSMs
def eoa : Address := "0x1111"
def creator : Address := "0x2222"
def signer : Address := "0x3333"
def domain : String := "test.com"

def passAccount : PassAccount := PassAccount.mkEmpty eoa creator

-- Test 1: Basic GSM domain ownership
def testGSMOwnership : Bool :=
  let gsm := passAccount.messageAsset
  -- Creator should be the initial signer for any domain
  gsm.getSigner domain = creator

-- Test 2: Transfer domain ownership
def testGSMTransfer : PassAccount × Bool :=
  passAccount.transferGSMDomain domain creator signer

-- Test 3: Process inbound message
def testMessage : WalletConnectorMessage := {
  domain := domain,
  message := "Hello World",
  signature := ""
}

def testInboundMessage : PassAccount × Bool :=
  let (pa, _) := testGSMTransfer
  pa.processInboundMessage testMessage

-- Test 4: Sign message internally
def testInternalSign : PassAccount × Bool :=
  let (pa, _) := testInboundMessage
  pa.internalSignGSM signer testMessage

-- Test 5: Sign all messages in outbox
def testOutboxSign : PassAccount × List WalletConnectorMessage :=
  let (pa, _) := testInternalSign
  pa.outboxSignGSM

-- Run tests
#eval testGSMOwnership
#eval testGSMTransfer
#eval testInboundMessage
#eval testInternalSign
#eval testOutboxSign
