-- This module serves as the root of the `LiqLeanPolicy` library.
-- Import modules here that should be built as part of the library.
import LiqLeanPolicy.PassAccount

def main : IO Unit := do
  let asset1 : Asset := { id := "ether",
                          assetType := AssetType.Ether,
                          balanceMap := [("0x1234", 100)] }
  IO.println (reprStr asset1)
  #check asset1
  IO.println "Test Complete"
