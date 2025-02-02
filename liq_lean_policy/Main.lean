-- TODO: first make this work with ether wei.
-- TODO: then make this work with tokens.
-- TODO: then make this work with GSM.


-- import LiqLeanPolicy.Basic

-- def a : Address := "0xabcd"

-- def account : Account := {
--   address := a,
--   owner := a
-- }

-- def subaccount : SubAccount := {
--   domain := account,
--   identifier := "0x1234"
-- }

-- theorem test : account.address = a := by
--   rfl

-- #check account.address
-- def addr_string : String := account.address

-- def main : IO Unit := do
--   IO.println s!"{subaccount.identifier}@{addr_string}"
--   #check addr_string
-- #check main
