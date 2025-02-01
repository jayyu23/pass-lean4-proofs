import Mathlib.Data.Finmap

inductive AssetType
  | Ether -- ETH is a special asset case
  | Token -- Non-ETH Token contract
  | GSM -- General Signable Message
  deriving Repr, BEq, DecidableEq

inductive TxStep
  | externalIn (txHash : String)
  | internalTransfer (fromSubaccount : String) -- TODO: model as Subaccount
  -- | gsmRequest (domain : String) -- Don't model GSM yet

structure Asset where
  id : String
  assetType : AssetType
  divisible : Bool
  provenanceChain : List TxStep
  totalBalance : Nat
  -- balanceMap : Finmap (λ _ : String ↦ Nat) -- Subaccount -> Balance

def externalIn (txHash : String) (asset : Asset) : Asset :=
{ asset with provenanceChain := TxStep.externalIn txHash :: asset.provenanceChain }

def internalTransfer (fromSubaccount : String) (asset : Asset) : Asset :=
  { asset with provenanceChain := TxStep.internalTransfer fromSubaccount :: asset.provenanceChain }

-- Define valid provenance chain. Must end with an externalIn step
def hasValidProvenance (asset : Asset) : Prop :=
  match asset.provenanceChain with
  | [] => False
  | [TxStep.externalIn _] => True
  | _ => False

-- Test this
example (txHash : String) (a : Asset) (h : a.provenanceChain = []) :
  hasValidProvenance (externalIn txHash a) := by
  -- Simplify the definition of `hasValidProvenance` after applying `externalIn`
  simp [externalIn, hasValidProvenance]
  -- After substitution, the provenance chain is [TxStep.externalIn txHash]
  simp [h]

-- example (fromSubaccount : String) (a : Asset) (h : hasValidProvenance a) :
--   hasValidProvenance (internalTransfer fromSubaccount a) := by
--   -- Decompose the validity condition of the original asset `a`

--   cases' h with h₁ h₂ <;> simp_all [hasValidProvenance]
--   -- Case where the provenance chain is empty, which contradicts the assumption that `a` has valid provenance
--   <;> induction' h₁ with hd tl <;> cases hd <;>
--     -- Ensure each step correctly updates the provenance chain when applying internal transfer
--     simp_all [internalTransfer, hasValidProvenance]
--   -- After handling all subcases, ensure the final state is correct
--   <;> rfl
--   -- Final check to ensure all conditions are met
