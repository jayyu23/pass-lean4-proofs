import PassWalletModels.PassAccount
import PassWalletModels.Asset
import Mathlib.Tactic.Basic

-- Basic theorems about PassAccount properties
theorem empty_account_has_no_assets (eoa creator : Address) :
  (PassAccount.mkEmpty eoa creator).assets = [] := by
  -- This should be trivially true by definition
  rfl

-- Balance non-negativity
theorem balance_non_negative (pa : PassAccount) (assetId : String) (addr : Address) :
  match pa.getAsset assetId with
  | none => true
  | some asset => asset.getBalance addr ≥ 0 := by
  cases pa.getAsset assetId with
  | none => trivial
  | some asset =>
    -- Balance is Nat which is always ≥ 0
    exact Nat.zero_le (asset.getBalance addr)
