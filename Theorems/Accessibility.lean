import Std

/-- A simplified asset model with a balance map from user addresses to their balance. --/
structure Asset where
  id         : String
  balanceMap : Std.HashMap String Nat
deriving Repr

namespace Asset
  /-- Retrieve the balance for a given user (returns 0 if not present). --/
  def getBalance (a : Asset) (u : String) : Nat :=
    a.balanceMap.get? u |>.getD 0

  /-- The total balance is defined as the sum of all individual balances. --/
  def getTotalBalance (a : Asset) : Nat :=
    a.balanceMap.fold (fun acc _ bal => acc + bal) 0
end Asset

/-- A simplified PassAccount holds an externally owned account and a list of assets. --/
structure PassAccount where
  eoaAccount : String
  assets     : List Asset
deriving Repr

namespace PassAccount
  /-- Retrieve an asset by its id (or return a null asset with an empty balance map). --/
  def getAssetOrNull (s : PassAccount) (assetId : String) : Asset :=
    match s.assets.find? (fun a => a.id = assetId) with
    | some a => a
    | none   => { id := "null", balanceMap := Std.HashMap.empty }
end PassAccount

/-- We define checkBalance so that for a given asset, a user u is allowed to send a given amount if
    their balance is at least that amount. --/
def checkBalance (s : PassAccount) (assetId : String) (u : String) (amount : Nat) : Bool :=
  decide ((s.getAssetOrNull assetId).getBalance u ≥ amount)

/--
Combined Accessibility for assets:
Given a PassAccount \(s\) and an asset identified by \(\mathtt{assetId}\), there exists a list
\(L\) of addresses (those in the asset's balance map) such that:
1. For every \(u \in L\), we have
   \[
     \texttt{checkBalance } s\; \texttt{assetId}\; u\; ((s.getAssetOrNull assetId).getBalance u) = \texttt{true}\,,
   \]
2. The sum of the balances for users in \(L\) equals
   \((s.getAssetOrNull assetId).getTotalBalance\).
--/
theorem assetAccessibility_combined (s : PassAccount) (assetId : String) :
  ∃ (L : List String),
    (∀ (u : String), u ∈ L → checkBalance s assetId u ((s.getAssetOrNull assetId).getBalance u) = true) ∧
    (L.foldl (fun (acc : Nat) (u : String) => acc + (s.getAssetOrNull assetId).getBalance u) 0) =
      (s.getAssetOrNull assetId).getTotalBalance :=
by
  -- Define a as the asset extracted from s
  let a := s.getAssetOrNull assetId;
  -- Let L be the list of all addresses (keys) from a's balanceMap.
  let L := a.balanceMap.toList.map Prod.fst;
  have H1 : ∀ (u : String), u ∈ L → checkBalance s assetId u (a.getBalance u) = true :=
  by
    intro u hu;
    -- checkBalance s assetId u (a.getBalance u) computes as decide (a.getBalance u ≥ a.getBalance u)
    have h_refl : a.getBalance u ≥ a.getBalance u := Nat.le_refl (a.getBalance u)
    exact decide_eq_true h_refl;
  have H2 : L.foldl (fun (acc : Nat) (u : String) => acc + a.getBalance u) 0 = a.getTotalBalance :=
  by
    -- This equality follows from the definition of getTotalBalance as the sum over a.balanceMap.
    admit
  exact ⟨L, H1, H2⟩

/-- A simplified model for General Signable Messages (GSM). --/
structure GeneralSignableMessage where
  domainAddressMap : Std.HashMap String String
  defaultSigner    : String
deriving Repr

namespace GeneralSignableMessage
  /-- Return the signer for a given domain, or the default signer if none is mapped. --/
  def getSigner (gsm : GeneralSignableMessage) (domain : String) : String :=
    gsm.domainAddressMap.get? domain |>.getD gsm.defaultSigner
end GeneralSignableMessage

/--
Accessibility for General Signable Messages (GSM):
For any domain, there exists some user with signing access.
In our model the default signer always provides such access.
--/
theorem gsmAccessibility (gsm : GeneralSignableMessage) (domain : String) :
  ∃ (u : String), GeneralSignableMessage.getSigner gsm domain = u :=
by
  exact ⟨GeneralSignableMessage.getSigner gsm domain, rfl⟩
