import LiqLeanPolicy.PassAccount

def asset1 : Asset := {
  id := "ether",
  assetType := AssetType.Ether,
  balanceMap := []
}

def passAccount1 : PassAccount := {
  id := "pass1",
  subaccounts := [{ id := "sub1", owner := "0x1234" }],
  eoaAccount := "0x1234",
  assets := [asset1],
  inbox := { id := "inbox1", owner := "0x1234", claimMap := [] },
  outbox := { id := "outbox1", owner := "0x1234", txQueue := [], nonce := 0 }
}

#eval Asset.getBalance asset1 "0x1234" -- Should be 0
#check passAccount1
