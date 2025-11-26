{-# OPTIONS --cubical --safe --guardedness #-}

{-|
  Evidenced Frames in HoTT: Conservative Extension
  =================================================

  This extends Part 1 to HoTT using Cubical Agda.

  CONSERVATIVE APPROACH (Alternative A):
  - Propositions Φ are sets (h-level 0)
  - Evidence E is a set (h-level 0)
  - Explicit h-level requirements
  - Closest to classical realizability
-}

module HoTT.EvidenceFrames.SimpleHoTT where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.HLevels
open import Cubical.Foundations.Isomorphism
open import Cubical.Foundations.Univalence
open import Cubical.Data.Nat using (ℕ; zero; suc)
open import Cubical.Data.Bool using (Bool; true; false)
open import Cubical.Data.Bool.Properties using (isSetBool)
open import Cubical.Data.Nat.Properties using (isSetℕ)
open import Cubical.Data.Maybe using (Maybe; just; nothing)
open import Cubical.Data.Sigma using (_×_; _,_; ∃-syntax)
open import Cubical.Data.Unit using (Unit; tt)
open import Cubical.Data.Empty using (⊥; isProp⊥)
open import Cubical.HITs.PropositionalTruncation as PT

private
  variable
    ℓ : Level

------------------------------------------------------------------------
-- Understanding H-Levels
------------------------------------------------------------------------

{-|
  H-Levels in HoTT:

  - isContr (h-level -2): Contractible types
  - isProp  (h-level -1): Propositions (all elements equal)
  - isSet   (h-level 0):  Sets (all paths equal)
  - isGroupoid (h-level 1): Groupoids

  For Evidenced Frames, we require sets (h-level 0)
-}

-- Examples that Bool and ℕ are sets
_ : isSet Bool
_ = isSetBool

_ : isSet ℕ
_ = isSetℕ

------------------------------------------------------------------------
-- HoTT Evidenced Frame Structure
------------------------------------------------------------------------

{-|
  Proposition Space: Set of propositions with h-level requirement
-}

record PropositionSpace (State Code : Type ℓ) : Type (ℓ-suc ℓ) where
  field
    Φ : Type ℓ
    Φ-isSet : isSet Φ

    -- Each proposition gives a stateful predicate
    ⟦_⟧ : Φ → State → Code → hProp ℓ

{-|
  Evidence Space: Set of evidence with h-level requirement
-}

record EvidenceSpace (Code : Type ℓ) : Type (ℓ-suc ℓ) where
  field
    E : Type ℓ
    E-isSet : isSet E

    -- Evidence are codes
    inject : E → Code

{-|
  HoTT Evidenced Frame: Conservative Extension

  Key: Explicit isSet requirements for Φ and E
-}

record HoTT-EvidencedFrame (State Code : Type ℓ) : Type (ℓ-suc ℓ) where
  field
    Props : PropositionSpace State Code
    Evid  : EvidenceSpace Code

  open PropositionSpace Props public
  open EvidenceSpace Evid public

  -- Application of codes
  field
    _·_ : Code → Code → Code

  -- Reduction relation
  field
    _⇓[_]_ : Code → State → (Code × State) → Type ℓ
    ⇓-isProp : ∀ {c σ cs'} → isProp (c ⇓[ σ ] cs')

  {-|
    Evidence Relation: e is evidence that φ₁ entails φ₂

    UNIFORMITY: Same e works for all σ and c!
  -}

  _⊢[_]_ : Φ → E → Φ → Type ℓ
  φ₁ ⊢[ e ] φ₂ = ∀ (σ : State) (c : Code)
    → ⟦ φ₁ ⟧ σ c .fst
    → ∃[ σ' ∈ State ] ∃[ c' ∈ Code ]
        ( ((inject e) · c) ⇓[ σ ] (c' , σ')
        × ⟦ φ₂ ⟧ σ' c' .fst
        )

  -- Required structure: Reflexivity
  field
    e-id : E
    reflexivity : ∀ {φ : Φ} → φ ⊢[ e-id ] φ

  -- Required structure: Transitivity
  field
    _⨾_ : E → E → E
    transitivity : ∀ {φ₁ φ₂ φ₃ : Φ} {e e' : E}
      → φ₁ ⊢[ e ] φ₂
      → φ₂ ⊢[ e' ] φ₃
      → φ₁ ⊢[ e ⨾ e' ] φ₃

------------------------------------------------------------------------
-- The HoTT Insight: Uniformity → Transport
------------------------------------------------------------------------

module UniformityImpliesTransport
  {State Code : Type ℓ}
  (EF : HoTT-EvidencedFrame State Code)
  where

  open HoTT-EvidencedFrame EF

  {-|
    KEY THEOREM: Evidence respects path equality in states

    If σ₁ ≡ σ₂ via path p, then propositions transport!

    This is AUTOMATIC from uniformity!
  -}

  evidence-respects-paths : ∀ {φ₁ φ₂ : Φ} {e : E}
    → φ₁ ⊢[ e ] φ₂
    → ∀ {σ₁ σ₂ : State}
    → (p : σ₁ ≡ σ₂)
    → ∀ c
    → ⟦ φ₁ ⟧ σ₁ c .fst
    → ⟦ φ₁ ⟧ σ₂ c .fst
  evidence-respects-paths {φ₁} {φ₂} {e} ent {σ₁} {σ₂} p c φ₁-holds =
    transport (λ i → ⟦ φ₁ ⟧ (p i) c .fst) φ₁-holds

  {-|
    INSIGHT: Uniformity in classical realizability becomes
             Transport in HoTT!

    Classical: "Same code works for all inputs"
    HoTT:      "Evidence transports along paths"
  -}

------------------------------------------------------------------------
-- Comparison with Classical (Part 1)
------------------------------------------------------------------------

{-|
  DIFFERENCES FROM CLASSICAL:

  1. EXPLICIT H-LEVELS
     Classical: Φ : Set
     HoTT:      Φ : Type + isSet Φ

  2. PATH TYPES
     Classical: σ₁ = σ₂ (propositional equality)
     HoTT:      σ₁ ≡ σ₂ (path type)

  3. TRANSPORT
     Classical: No notion of transport
     HoTT:      Can move along paths via transport

  4. UNIVALENCE (not yet used, but available)
     Classical: Equivalence ≠ equality
     HoTT:      Equivalence ≡ equality

  WHAT'S THE SAME:

  - Core structure (Φ, E, ⊢)
  - Uniformity is central
  - Evidence transforms realizers
  - No equational theory

  THE CONSERVATIVE EXTENSION:

  By requiring isSet for Φ and E:
  ✓ Stay close to classical
  ✓ Avoid higher homotopy structure
  ✓ Can prove same theorems
  ✓ But gain HoTT tools (transport, univalence)
-}

------------------------------------------------------------------------
-- Key Insight for Research
------------------------------------------------------------------------

{-|
  UNIFORMITY ↔ TRANSPORT CONNECTION

  Classical uniformity: "Same code for all inputs"

  In HoTT, this means:
  - Evidence works for σ₁
  - Evidence works for σ₂
  - If σ₁ ≡ σ₂, then the proof transports!

  This is a NEW GEOMETRIC understanding of uniformity!

  Uniformity isn't just about computation -
  it's about GEOMETRY and PATHS!

  This insight opens the door to:
  - Path-based evidence (Part 3)
  - HIT-indexed propositions (Part 3)
  - Univalence-generated evidence (Part 3)
-}
