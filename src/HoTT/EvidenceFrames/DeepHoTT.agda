{-# OPTIONS --cubical --guardedness #-}

{-|
  Evidenced Frames in HoTT: Deep Integration with Higher Structures
  ==================================================================

  This module explores NOVEL research directions for Evidenced Frames in HoTT.

  Part 2 showed conservative extension (sets only).
  Part 3 explores what's UNIQUELY possible in HoTT.

  KEY RESEARCH QUESTIONS:
  1. Can HITs serve as propositions?
  2. Can paths themselves be evidence?
  3. How does univalence interact with uniformity?
  4. Can HITs be viewed as computational effects?

  This is RESEARCH - exploring uncharted territory!
-}

module HoTT.EvidenceFrames.DeepHoTT where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.HLevels
open import Cubical.Foundations.Isomorphism
open import Cubical.Foundations.Univalence
open import Cubical.Foundations.Equiv
open import Cubical.Data.Nat using (ℕ; zero; suc)
open import Cubical.Data.Bool using (Bool; true; false)
open import Cubical.Data.Sigma using (_×_; _,_; ∃-syntax)
open import Cubical.Data.Unit using (Unit; tt)
open import Cubical.Data.Empty using (⊥)
open import Cubical.HITs.S1 as S1
open import Cubical.HITs.PropositionalTruncation as PT

private
  variable
    ℓ : Level

------------------------------------------------------------------------
-- Research Direction 1: HITs as Propositions
------------------------------------------------------------------------

{-|
  THE CIRCLE AS A PROPOSITION SPACE

  Question: What if propositions have higher structure?

  Classical: Propositions are sets (h-level 0)
  Radical:   Propositions could be any h-level!

  The Circle S¹ has:
  - base : S¹
  - loop : base ≡ base (non-trivial path!)

  What could "the circle as a proposition" mean?
-}

module CircleAsProposition where

  -- A family of propositions parameterized by S¹
  CircleIndexedProposition : Type (ℓ-suc ℓ)
  CircleIndexedProposition {ℓ} = S1.S¹ → hProp ℓ

  {-|
    INTERPRETATION IDEAS:

    1. Propositions "rotate" as you go around the loop
    2. Code positions on the circle
    3. Periodic properties

    Example: "Code is at position x on the circle"
  -}

  -- Placeholder: What should this be?
  postulate
    at-position : CircleIndexedProposition {ℓ-zero}

  {-|
    RESEARCH QUESTION 1.1: Right definition?

    Options:
    a) Position is metadata on codes
    b) State is S¹ itself (circular state)
    c) Evidence moves you around the circle

    Each leads to different models!
  -}

------------------------------------------------------------------------
-- Research Direction 2: Path-Based Evidence
------------------------------------------------------------------------

{-|
  PATHS AS EVIDENCE

  Classical: Evidence e : E is a CODE
  HoTT:      Evidence could be a PATH!

  If p : φ₁ ≡ φ₂, then p IS evidence that φ₁ entails φ₂!

  Why: Realizers transport along the path!
-}

module PathBasedEvidence where

  {-|
    Path Evidenced Frame: Alternative C

    Evidence = paths between propositions
  -}

  record PathEvidencedFrame (State Code : Type ℓ) : Type (ℓ-suc ℓ) where
    field
      Φ : Type ℓ
      -- NO isSet requirement! Φ can have higher structure!

      -- Interpretation
      ⟦_⟧ : Φ → State → Code → Type ℓ

    {-|
      Evidence relation: A PATH is evidence!
    -}

    _⊢[_]_ : (φ₁ φ₂ : Φ) → (p : φ₁ ≡ φ₂) → Type ℓ
    _⊢[_]_ φ₁ φ₂ p = (φ₁ ≡ φ₂)

    {-|
      Entailment via Transport

      Given p : φ₁ ≡ φ₂, realizers transport!
    -}

    transport-realizer : ∀ {φ₁ φ₂ : Φ} (p : φ₁ ≡ φ₂) σ c
      → ⟦ φ₁ ⟧ σ c
      → ⟦ φ₂ ⟧ σ c
    transport-realizer p σ c = transport (λ i → ⟦ p i ⟧ σ c)

    {-|
      Transitivity = Path Concatenation (AUTOMATIC!)
    -}

    compose-evidence : ∀ {φ₁ φ₂ φ₃ : Φ}
      → (p : φ₁ ≡ φ₂)
      → (q : φ₂ ≡ φ₃)
      → (φ₁ ≡ φ₃)
    compose-evidence p q = p ∙ q

  {-|
    RESEARCH QUESTION 2.1: Connection to classical?

    Can we embed code-based evidence into path-based?

    Idea: Code e induces path p such that
          transport along p ≡ apply e

    Then: Classical evidence ⊆ Path evidence
  -}

------------------------------------------------------------------------
-- Research Direction 3: Univalence + Uniformity
------------------------------------------------------------------------

{-|
  UNIVALENCE: The Key HoTT Axiom

  (A ≃ B) ≃ (A ≡ B)
  Equivalent types are EQUAL!

  For EF: If propositions are equivalent, they're equal!
-}

module UnivalenceForPropositions where

  {-|
    Proposition Equivalence

    Two propositions equivalent if:
    - Realizers in bijection
    - Uniformly across all states
  -}

  record PropEquiv (State Code : Type ℓ)
                   (⟦_⟧₁ ⟦_⟧₂ : State → Code → Type ℓ)
                   : Type ℓ where
    field
      to   : ∀ σ c → ⟦_⟧₁ σ c → ⟦_⟧₂ σ c
      from : ∀ σ c → ⟦_⟧₂ σ c → ⟦_⟧₁ σ c

      to-from : ∀ σ c (r₂ : ⟦_⟧₂ σ c) → to σ c (from σ c r₂) ≡ r₂
      from-to : ∀ σ c (r₁ : ⟦_⟧₁ σ c) → from σ c (to σ c r₁) ≡ r₁

  {-|
    UNIVALENCE THEOREM:

    Proposition equivalence → Path between propositions!

    This path IS evidence (path-based)!

    Meaning: Univalence GENERATES evidence automatically!
  -}

  {-|
    RESEARCH QUESTION 3.1: Computational content?

    Problem: Univalence not obviously computational
    Solution: Cubical Agda makes it computational via Glue!

    For EF: Could give computational meaning to equivalence!
  -}

------------------------------------------------------------------------
-- Research Direction 4: HITs as Computational Effects
------------------------------------------------------------------------

{-|
  HIGHER INDUCTIVE TYPES AS EFFECTS

  Classical effects: state, non-determinism, failure
  HoTT effects: HITs add structure to types!

  Examples:
  - Truncation: Collapse equality (effect: forget information)
  - Quotient: Identify elements (effect: equality testing)
  - Circle: Add loop (effect: periodicity)
-}

module HITsAsEffects where

  {-|
    EXAMPLE 1: Propositional Truncation as Failure

    ∥ A ∥ collapses all elements
    Forgets WHICH element you have
    Only retains EXISTENCE

    Analogy to fail combinator:
    - fail terminates but no value
    - ∥ A ∥ witnesses existence but no element
  -}

  -- Placeholder for failing realizability
  postulate
    FailingRealizability : Type₁

  {-|
    EXAMPLE 2: Circle as Periodic Computation

    S¹ has base and loop : base ≡ base

    Computational interpretation:
    - base = initial state
    - loop = one full cycle
    - Going around n times = iterating n times

    Could model:
    - Periodic processes
    - Cyclic state machines
    - Modular arithmetic effects
  -}

  {-|
    EXAMPLE 3: Quotients as Equality Checking

    A / ~ identifies equivalent elements

    Computational interpretation:
    - Can test a ~ b
    - Effect: extensional equality checking
  -}

------------------------------------------------------------------------
-- Research Direction 5: Evidence with Higher Coherence
------------------------------------------------------------------------

{-|
  EVIDENCE AS TYPE FAMILY: Alternative B

  Classical: Evidence relation φ₁ →^e φ₂ is ternary
  Type Family: Evidence : Φ → Φ → Type

  This is CATEGORICAL:
  - Objects = propositions
  - Morphisms = evidence

  In HoTT: Need COHERENCE!
-}

module TypeFamilyEvidence where

  {-|
    Categorical Evidenced Frame

    Want:
    - Associativity: (e₁ ; e₂) ; e₃ ≡ e₁ ; (e₂ ; e₃)
    - Unit laws: id ; e ≡ e ≡ e ; id

    But in HoTT:
    - These might not hold definitionally!
    - Need PATHS witnessing associativity
    - And paths between those paths (2-coherence)
    - And so on... (∞-coherence!)

    This leads to ∞-CATEGORIES!
  -}

  record CategoricalEvidencedFrame (State Code : Type ℓ) : Type (ℓ-suc ℓ) where
    field
      Φ : Type ℓ
      Φ-isSet : isSet Φ

      -- Evidence is a TYPE FAMILY
      Evidence : Φ → Φ → Type ℓ

      -- Identity and composition
      id : ∀ {φ} → Evidence φ φ
      _∘_ : ∀ {φ₁ φ₂ φ₃} → Evidence φ₂ φ₃ → Evidence φ₁ φ₂ → Evidence φ₁ φ₃

      -- Coherence 1: Associativity
      assoc : ∀ {φ₁ φ₂ φ₃ φ₄}
            → (e₃ : Evidence φ₃ φ₄)
            → (e₂ : Evidence φ₂ φ₃)
            → (e₁ : Evidence φ₁ φ₂)
            → (e₃ ∘ e₂) ∘ e₁ ≡ e₃ ∘ (e₂ ∘ e₁)

      -- Coherence 2: Unit laws
      unit-left  : ∀ {φ₁ φ₂} (e : Evidence φ₁ φ₂) → id ∘ e ≡ e
      unit-right : ∀ {φ₁ φ₂} (e : Evidence φ₁ φ₂) → e ∘ id ≡ e

  {-|
    RESEARCH QUESTION 5.1: Higher coherences?

    - Are assoc proofs unique? (2-coherence!)
    - Pentagon axiom? (3-coherence!)
    - Weak ∞-category?

    Deep categorical homotopy theory!
  -}

------------------------------------------------------------------------
-- Research Direction 6: Synthetic Homotopy Realizability
------------------------------------------------------------------------

{-|
  GRAND VISION: Realizability for Synthetic Homotopy Theory

  Synthetic Homotopy Theory:
  - Define homotopy concepts (spheres, suspensions)
  - WITHOUT topological spaces
  - Using only type theory + HITs

  Synthetic Realizability:
  - Define realizability WITHOUT specific PCA
  - Using only evidenced frames + HoTT

  Could we have:
  - Realizers for homotopy groups?
  - Evidence for path spaces?
  - Computational interpretation of suspensions?
-}

module SyntheticRealizability where

  {-|
    EXAMPLE: Loop Space

    For type A with point a : A,
    loop space ΩA = (a ≡ a)

    Question: What are realizers for ΩA?

    Possible answers:
    1. Codes that "go around loops"
    2. Evidence that is itself a loop (path-based!)
    3. State transitions forming cycles
  -}

  {-|
    EXAMPLE: Homotopy Groups πₙ

    πₙ(A, a) = ∥ Sⁿ → A ∥₀

    Question: What does it mean to REALIZE homotopy group element?

    Speculation:
    - Realizer = code traversing n-sphere
    - Evidence = transformation between sphere maps
    - Uniformity = same code for all base points!

    COMPLETELY NOVEL - no existing work!
  -}

------------------------------------------------------------------------
-- Summary: Research Directions
------------------------------------------------------------------------

{-|
  PART 3 RESEARCH DIRECTIONS:

  1. HITs AS PROPOSITIONS
     - Circle, spheres with higher structure
     - Geometric realizability
     - Position as computation

  2. PATH-BASED EVIDENCE
     - Paths as evidence
     - Transport = entailment
     - More general than codes

  3. UNIVALENCE + UNIFORMITY
     - Equivalence generates evidence
     - Computational content via Glue
     - Uniformity up to equivalence

  4. HITs AS EFFECTS
     - Truncation ~ failure
     - Quotients ~ equality checking
     - Circle ~ periodic computation

  5. CATEGORICAL STRUCTURE
     - Evidence as type family
     - Coherence conditions
     - ∞-categorical realizability

  6. SYNTHETIC HOMOTOPY
     - Loop spaces with realizers
     - Homotopy groups computationally
     - Novel research area!

  KEY INSIGHTS:

  1. Uniformity has GEOMETRIC meaning (transport)
  2. HoTT gives NEW effects (HITs)
  3. Evidence is RICHER (codes, paths, families)
  4. Deep connections: Realizability ↔ Homotopy

  WHY THIS MATTERS:

  - Unify realizability and homotopy theory
  - Computational meaning for synthetic homotopy
  - New models of computation
  - Deepen understanding of effects

  NEXT: Part 4 - Research Questions for Liron
-}
