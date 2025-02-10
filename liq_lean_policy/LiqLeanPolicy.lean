-- This module serves as the root of the `LiqLeanPolicy` library.
-- Import modules here that should be built as part of the library.
import LiqLeanPolicy.PassAccount

def asset1 : Asset := {
  id := "ether",
  assetType := AssetType.Ether,
  balanceMap := [("0x1234", 100)]
}

#eval Asset.getBalance asset1 "0x1234"
