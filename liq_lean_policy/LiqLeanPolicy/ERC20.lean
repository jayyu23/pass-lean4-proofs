import LiqLeanPolicy.Basic

-- Rewrite

-- Core token state
def Balance := Nat
deriving DecidableEq, Repr, ToString


-- instance [OfNat Nat n] : OfNat Balance n where ofNat := n
-- instance : LE Balance where le := Nat.le

-- structure TokenState where
--   balances : List (Address × Balance)

-- def getBalance (self : TokenState) (address : Address) : Balance :=
--   match self.balances.find? (fun (addr, _) => addr = address) with
--   | some (_, bal) => bal
--   | none => Nat.zero

-- def canExternalTransfer (self : TokenState) (transfer : ExternalTransfer) : Prop :=
--   getBalance self transfer.fromAccount.address >= transfer.amount

-- theorem test_transfer (state : TokenState) (t : ExternalTransfer) :
--   canExternalTransfer state t →
--   t.amount ≤ getBalance state t.fromAccount.address := by
--   intro h
--   exact h
