-- import LiqLeanPolicy

-- def main : IO Unit := do
  -- Create test addresses
  -- let addr1 := Address.mk "0x1234"
  -- let addr2 := Address.mk "0x5678"

  -- -- Create test accounts
  -- let account1 := Account.mk addr1 addr1
  -- let account2 := Account.mk addr2 addr2

  -- -- Initialize token state with some balances
  -- let initialState := TokenState.mk [
  --   (addr1, (1000 : Nat)),
  --   (addr2, (500 : Nat))
  -- ]

  -- -- Create a test transfer
  -- let transfer := ExternalTransfer.mk account1 addr2 100

  -- -- Test balance checking
  -- let senderBalance := getBalance initialState addr1
  -- IO.println s!"{addr1}"

def main : IO Unit := do
IO.println "Hello, World!"
