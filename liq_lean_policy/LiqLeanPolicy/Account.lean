import LiqLeanPolicy.EVMState


-- Create definition of LiqAccount, which has an encumbered private key
structure LiqAccount extends Account where
  -- subAccounts : List SubAccount
  accessManager : Address
