import PassWalletModels.PassAccount
import PassWalletModels.Asset
import PassWalletModels.EVMState
import Mathlib.Tactic.Basic
import Std -- needed for Std.HashMap, fold, etc.

open Std -- so that HashMap and List.foldl, etc. can be accessed more directly

namespace ProvenanceIntegrity

/--
  Helper function to calculate total claimed amount for an asset from the provenance history.
-/
def totalClaimed (pa : PassAccount) (assetId : String) : Nat :=
  pa.getProvHistory assetId
  |> List.filter (fun tx => tx.txType = TransactionType.claim)
  |> List.foldl (fun sum tx => sum + tx.amount) 0

/--
  Helper function to calculate total withdrawn amount for an asset from the provenance history.
-/
def totalWithdrawn (pa : PassAccount) (assetId : String) : Nat :=
  pa.getProvHistory assetId
  |> List.filter (fun tx => tx.txType = TransactionType.withdraw)
  |> List.foldl (fun sum tx => sum + tx.amount) 0

/--
  Helper function to calculate total transferred amount for an asset (internal transfers).
-/
def totalTransferred (pa : PassAccount) (assetId : String) : Nat :=
  pa.getProvHistory assetId
  |> List.filter (fun tx => tx.txType = TransactionType.transfer)
  |> List.foldl (fun sum tx => sum + tx.amount) 0

/--
  Helper to calculate net external deposits (claimed - withdrawn).
-/
def netExternalDeposits (pa : PassAccount) (assetId : String) : Nat :=
  let claimed := totalClaimed pa assetId
  let withdrawn := totalWithdrawn pa assetId
  if claimed ≥ withdrawn then claimed - withdrawn else 0

/--
  Provenance Integrity Theorem:
  For any PASS account and any asset, the total amount of the asset
  held across all subaccounts can never exceed the total amount of
  external deposits minus withdrawals.
-/
theorem provenance_integrity (pa : PassAccount) (assetId : String) :
  let asset := pa.getAssetOrNull assetId
  let totalSubaccountBalance := asset.getTotalBalance
  let netDeposits := netExternalDeposits pa assetId
  totalSubaccountBalance ≤ netDeposits := by
  -- We'll prove this by (sketched) induction on the provenance history.

  let asset := pa.getAssetOrNull assetId
  let totalBalance := asset.getTotalBalance
  let netDeposits := netExternalDeposits pa assetId

  -- Base case: empty history
  if h_empty : pa.provHistory.isEmpty then
    have h_balance_zero : totalBalance = 0 := by
      -- If provHistory is empty, then either asset doesn't exist or is empty
      have h_asset : pa.getAssetOrNull assetId = Asset.mkEmpty assetId := by
        unfold PassAccount.getAssetOrNull
      rw [h_asset]
      simp [Asset.getTotalBalance]

    have h_deposits_zero : netDeposits = 0 := by
      dsimp [netExternalDeposits, totalClaimed, totalWithdrawn]
      -- pa.getProvHistory assetId = [] in this branch
      simp [h_empty]

    simp [h_balance_zero, h_deposits_zero]
  else
    -- Inductive case: prove each operation preserves totalBalance ≤ netDeposits
    sorry

/--
  Statement of provenance integrity explicitly via summing all subaccount balances.
-/
theorem provenance_integrity_sum (pa : PassAccount) (assetId : String) :
  let asset := pa.getAssetOrNull assetId
  let allAddresses := asset.balanceMap.toList.map Prod.fst
  let sumBalances := allAddresses.foldl (fun acc addr => acc + asset.getBalance addr) 0
  let netDeposits := netExternalDeposits pa assetId
  sumBalances ≤ netDeposits := by

  let asset := pa.getAssetOrNull assetId
  let allAddresses := asset.balanceMap.toList.map Prod.fst
  let sumBalances := allAddresses.foldl (fun acc addr => acc + asset.getBalance addr) 0
  let netDeposits := netExternalDeposits pa assetId

  -- show sumBalances = asset.getTotalBalance
  have h_sum_eq_total : sumBalances = asset.getTotalBalance := by
    dsimp [Asset.getTotalBalance]
    -- prove that folding over the map's keys = totalBalance
    sorry

  rw [h_sum_eq_total]
  exact provenance_integrity pa assetId

end ProvenanceIntegrity
