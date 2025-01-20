import Mathlib.Data.Finmap
import LiqLeanPolicy.EVMState
import Mathlib.Order.Basic

/- Basic RBAC primitives -/
inductive Permission
  | Send       -- Can send assets
  | Receive    -- Can receive assets
  | Trade      -- Can trade on DEXs
  | Vote       -- Can vote in governance
  | Delegate   -- Can delegate permissions
  | Admin      -- Full administrative rights
  deriving Repr, BEq

structure Role where
  name: String
  permissions: List Permission
  deriving Repr, BEq

structure TimeConstraint where
  startTime: Nat
  endTime: Nat
  deriving Repr, BEq

/- Asset constraints for roles -/
structure AssetConstraint where
  assetType: String        -- "ETH", "ERC20", etc
  maxAmount: Option Wei    -- Maximum amount that can be controlled
  deriving Repr, BEq

/- Role assignment with constraints -/
structure RoleAssignment where
  role: Role
  timeConstraint: TimeConstraint
  assetConstraints: List AssetConstraint
  deriving Repr, BEq

/- Subject (user or subaccount) -/
structure Subject where
  id: String              -- Subaccount identifier
  address: Address        -- Controller address
  roles: List RoleAssignment
  parent: Option String   -- Parent subaccount id
  deriving Repr, BEq

/- RBAC-based Liquefaction Account -/
structure RBACLiquefactionAccount where
  address: Address
  teeAttestation: List Nat
  encumberedPrivateKey: List Nat
  accessManager: Address

  -- RBAC components
  roles: List Role                    -- Available roles
  subjects: Finmap (λ _ : String ↦ Subject)     -- Active subjects
  roleHierarchy: Finmap (λ _ : String ↦ String) -- Role inheritance

  -- State tracking
  nonce: Nat
  spentBalances: Finmap (λ _ : String ↦ Wei)    -- Per-subject spent amounts

/- Check if a subject has a specific permission -/
def hasPermission (account: RBACLiquefactionAccount)
    (subjectId: String) (perm: Permission) (time: Nat) : Bool :=
  match Finmap.lookup subjectId account.subjects with
  | none => false
  | some subject =>
      -- Check direct role permissions
      let hasDirectPermission := subject.roles.any fun assignment =>
        assignment.role.permissions.elem perm &&
        assignment.timeConstraint.startTime ≤ time &&
        time ≤ assignment.timeConstraint.endTime

      -- Check inherited permissions through role hierarchy
      let hasInheritedPermission := subject.roles.any fun assignment =>
        let rec checkInherited (roleName: String) (depth: Nat) : Bool :=
          if depth = 0 then false else
            match Finmap.lookup roleName account.roleHierarchy with
            | none => false
            | some parentRole =>
                let parentRoleObj := account.roles.find? (·.name = parentRole)
                match parentRoleObj with
                | none => false
                | some role =>
                    role.permissions.elem perm || checkInherited parentRole (depth - 1)
        termination_by depth

        checkInherited assignment.role.name 100

      hasDirectPermission || hasInheritedPermission

/- Create predefined roles -/
def createBasicRoles : List Role := [
  { name := "admin"
    permissions := [Permission.Admin, Permission.Delegate]
  },
  { name := "trader"
    permissions := [Permission.Trade, Permission.Send, Permission.Receive]
  },
  { name := "viewer"
    permissions := []
  }
]

/- Example: Create trading subaccount with RBAC -/
-- def createTradingSubaccount (account: RBACLiquefactionAccount)
--     (traderId: String) (trader: Address) : RBACLiquefactionAccount :=
--   let traderRole : RoleAssignment := {
--     role := { name := "trader"
--              permissions := [Permission.Trade, Permission.Send, Permission.Receive] }
--     timeConstraint := { startTime := 0, endTime := 1000000 }
--     assetConstraints := [
--       { assetType := "ETH"
--         maxAmount := some 1000000 }
--     ]
--   }

--   let subject : Subject := {
--     id := traderId
--     address := trader
--     roles := [traderRole]
--     parent := some "root"
--   }

--   { account with
--     subjects := account.subjects.insert traderId subject
--   }



-- /- Check for balance constraints -/
-- def checkBalanceConstraints (account: RBACLiquefactionAccount)
--     (subjectId: String) (amount: Wei) (assetType: String) : Bool :=
--   match Finmap.lookup subjectId account.subjects with
--   | none => false
--   | some subject =>
--       subject.roles.all fun assignment =>
--         assignment.assetConstraints.all fun constraint =>
--           if constraint.assetType = assetType
--           then match constraint.maxAmount with
--                | none => true
--                | some max =>
--                    match Finmap.lookup subjectId account.spentBalances with
--                    | none => amount <= max
--                    | some spent => spent + amount <= max
--           else true
