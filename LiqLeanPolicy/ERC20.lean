structure ERC20 where
  name : String
  symbol : String
  totalBalance : Nat
  delegatedAmount : Nat -- 1000 USDC delegated to Alice
  deriving DecidableEq

def ERC20.totalSupply (self : ERC20) : Nat :=
  self.totalBalance - self.delegatedAmount
