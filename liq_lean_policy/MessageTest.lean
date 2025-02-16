import LiqLeanPolicy.PassAccount
import LiqLeanPolicy.Asset


-- TODO: Test out GSMs
def eoa : Address := "0x1111"
def passAccount : PassAccount := PassAccount.mkEmpty eoa

def testGSM1 : Address :=
  let gsm := passAccount.messageAsset
  gsm.getSigner "test.com"

def testGSM2 : Address :=
  let gsm := passAccount.messageAsset
  let gsm2 := gsm.setDomain "test.com" "0x2222"
  gsm2.getSigner "test.com"


#eval testGSM2
