{-
  PCA UNIT TESTS - Understanding PCAs through Examples

  This file contains concrete examples and tests to help you understand:
  1. How PCAs work
  2. How the S and K combinators behave
  3. How to build derived combinators
  4. The connection to Evidenced Frames

  Each test is a theorem that Agda verifies by computation.
  The fact that these compile means the tests pass!
-}

module PCA.PCAUnitTests where

open import Agda.Builtin.Maybe
open import Agda.Builtin.Equality
open import Agda.Builtin.Nat
open import Agda.Builtin.Unit
open import Agda.Builtin.Sigma
open import PCA.PCA

{-
  PART 1: A Concrete PCA - Simple Combinatory Calculus

  Let's build a simple PCA using a concrete syntax tree representation.
  This makes everything tangible and testable.
-}

-- Our codes are just S, K, and applications
data Code : Set where
  K-code : Code
  S-code : Code
  App    : Code → Code → Code

-- Helper for case expressions
case_of_ : {A B : Set} → A → (A → B) → B
case x of f = f x

-- Reduction with a step counter (to ensure termination)
reduce : Nat → Code → Code → Maybe Code
reduce zero f x = nothing  -- Out of steps = divergence
reduce (suc n) K-code x = just (App K-code x)  -- K x is a value
reduce (suc n) (App K-code x) y = just x   -- K x y → x  (K LAW!)
reduce (suc n) S-code x = just (App S-code x)  -- S x is a value
reduce (suc n) (App S-code x) y = just (App (App S-code x) y)  -- S x y is a value
reduce (suc n) (App (App S-code x) y) z =  -- S x y z → (x z)(y z)  (S LAW!)
  case reduce n x z of λ where
    nothing → nothing
    (just xz) → case reduce n y z of λ where
      nothing → nothing
      (just yz) → reduce n xz yz
reduce (suc n) (App f x) y =
  case reduce n f x of λ where
    nothing → nothing
    (just fx) → reduce n fx y

-- Application operation for our PCA
_apply_ : Code → Code → Maybe Code
f apply x = reduce 100 f x  -- Use 100 steps as enough

{-
  PART 2: Verifying the PCA Laws

  Now we verify that our Code satisfies the PCA laws.
  These proofs are "by computation" - Agda just evaluates and checks!
-}

-- TEST 1: K Law - K x y reduces to x
test-k-law-1 : K-code apply K-code ≡ just (App K-code K-code)
test-k-law-1 = refl

test-k-law-2 : (App K-code K-code) apply S-code ≡ just K-code
test-k-law-2 = refl

-- TEST 3: K Law with different values
test-k-law-3 : K-code apply S-code ≡ just (App K-code S-code)
test-k-law-3 = refl

test-k-law-4 : (App K-code S-code) apply K-code ≡ just S-code
test-k-law-4 = refl

{-
  PART 3: Building the Identity Combinator

  One of the most important results: I = S K K
  This shows that identity can be built from S and K alone!
-}

-- Define I combinator: I = S K K
I-combinator : Code
I-combinator = App (App S-code K-code) K-code

-- TEST 4: I x reduces to x (test with K)
test-identity-K : I-combinator apply K-code ≡ just K-code
test-identity-K = refl

-- TEST 5: I x reduces to x (test with S)
test-identity-S : I-combinator apply S-code ≡ just S-code
test-identity-S = refl

-- TEST 6: I (S K) = S K (identity preserves complex terms)
test-identity-complex :
  I-combinator apply (App S-code K-code) ≡ just (App S-code K-code)
test-identity-complex = refl

{-
  Let's trace through why I K = K:

  I K = (S K K) K             [definition of I]
      = (K K (K K))           [S law: S x y z = (x z)(y z)]
      = K                     [K law: K x y = x]

  This is the ESSENCE of functional completeness!
  We built identity using only S and K!
-}

{-
  PART 4: Church Encodings - Encoding Data as Functions

  PCAs only have functions. To represent data, we use Church encodings.
-}

-- Church booleans: true = K, false = K I
church-true : Code
church-true = K-code

church-false : Code
church-false = App K-code I-combinator

-- TEST 7: true x y = x (K takes first argument)
test-church-true-1 : church-true apply S-code ≡ just (App K-code S-code)
test-church-true-1 = refl

test-church-true-2 : (App K-code S-code) apply K-code ≡ just S-code
test-church-true-2 = refl

-- TEST 8: false x y = y (K I takes second argument)
-- church-false = K I, so (K I) S reduces to I (using K law)
test-church-false-1 :
  church-false apply S-code ≡ just I-combinator
test-church-false-1 = refl

-- When false is applied to two arguments, it returns the second
-- false x y = (K I) x y = I y = y

{-
  Why does false work?

  false x y = (K I) x y
            = I y           [K law: K I x = I]
            = y             [I law: I y = y]

  So false ignores first argument and returns second!
-}

{-
  PART 5: Church Numerals - Encoding Natural Numbers

  A Church numeral n is a function that applies f to x exactly n times.

  0 = λf.λx.x           (apply f zero times)
  1 = λf.λx.f x         (apply f once)
  2 = λf.λx.f (f x)     (apply f twice)
-}

-- Church numeral 0 = K I = λf.λx.x
church-0 : Code
church-0 = App K-code I-combinator

-- Church numeral 1 = I = λf.λx.f x
church-1 : Code
church-1 = I-combinator

-- TEST 9: Church 0 applied to function returns identity
-- church-0 = K I, so (K I) K reduces to I (using K law)
test-church-0 : church-0 apply K-code ≡ just I-combinator
test-church-0 = refl

-- TEST 10: Church 1 is just identity combinator
test-church-1 : church-1 apply K-code ≡ just K-code
test-church-1 = refl

{-
  PART 6: Understanding Uniformity (Key for Realizability!)

  UNIFORMITY is crucial for realizability models. It means:
  - The SAME code works for ALL inputs
  - We can't "peek" at the input and choose different codes

  Example: The identity combinator I works uniformly for any input.
-}

-- TEST 11: Uniformity - I works the same way on different inputs
test-uniformity-1 : I-combinator apply K-code ≡ just K-code
test-uniformity-1 = refl

test-uniformity-2 : I-combinator apply S-code ≡ just S-code
test-uniformity-2 = refl

test-uniformity-3 : I-combinator apply I-combinator ≡ just I-combinator
test-uniformity-3 = refl

{-
  Why uniformity matters for Evidenced Frames:

  In an evidenced frame, we have propositions φ and evidence e that φ₁ → φ₂.
  The evidence e must work UNIFORMLY for all realizers of φ₁.

  If φ₁ is "true for input n", then e must convert ALL realizers of φ₁
  to realizers of φ₂, using the SAME code e for all n.

  This is why PCAs use S and K - they provide UNIFORM computation!
-}

{-
  PART 7: Connection to Evidenced Frames

  From the paper (Example III.4), a PCA naturally forms an evidenced frame:

  • Propositions Φ = P(C)  (sets of codes)
  • Evidence E = C         (codes themselves)
  • Evidence relation: c evidences φ₁ ⊢ φ₂ when:
      for all c₁ ∈ φ₁, we have (c · c₁) terminates and (c · c₁) ∈ φ₂

  Let's demonstrate this with concrete examples:
-}

-- A simple proposition: "the code equals K"
is-K-code : Code → Set
is-K-code K-code = ⊤
is-K-code _ = ⊥
  where
    data ⊥ : Set where

-- Evidence that "is-K ⊢ is-K": the identity combinator!
-- For any code c that is K, I · c will also be K
evidence-identity-preserves-K :
  (c : Code) → is-K-code c →
  Σ Code (λ result → Σ (I-combinator apply c ≡ just result) (λ _ → is-K-code result))
evidence-identity-preserves-K K-code tt = K-code , (refl , tt)
evidence-identity-preserves-K S-code ()
evidence-identity-preserves-K (App c c₁) ()

{-
  This example shows:
  - φ₁ = "codes that equal K"
  - φ₂ = "codes that equal K"
  - e = I (the identity combinator)
  - I is evidence that φ₁ ⊢ φ₂ because I · K = K

  In realizability, this corresponds to:
  "If we have a proof that something is K,
   then I transforms it into another proof that it's K"
-}

{-
  PART 8: Testing Composition (Transitivity of Evidence)

  In an evidenced frame, evidence can be composed.
  If e₁ : φ₁ → φ₂ and e₂ : φ₂ → φ₃, then e₂ ∘ e₁ : φ₁ → φ₃

  In PCAs, composition is just λx. e₂ (e₁ x)
-}

-- B combinator (composition): B f g x = f (g x)
-- B = S (K S) K
B-combinator : Code
B-combinator = App (App S-code (App K-code S-code)) K-code

-- TEST 12: B composes correctly: B I K x = I (K x) = K x
test-composition :
  let step1 = B-combinator apply I-combinator
      step2 = case step1 of λ where
        nothing → nothing
        (just bi) → bi apply K-code
      step3 = case step2 of λ where
        nothing → nothing
        (just bik) → bik apply S-code
  in step3 ≡ just (App K-code S-code)
test-composition = refl

{-
  PART 9: Summary and Key Insights

  What you've learned:

  1. PCAs provide a minimal model of computation (just S, K, and application)

  2. S and K are UNIVERSAL - any computable function can be built from them

  3. Church encodings let us represent data (booleans, numbers) as functions

  4. UNIFORMITY is key - the same code must work for all inputs

  5. PCAs naturally form evidenced frames:
     - Propositions = sets of codes
     - Evidence = codes that transform realizers
     - This is the foundation of realizability!

  6. In realizability models:
     - A proposition is "true" if it has a realizer (a code in the PCA)
     - Implication φ₁ ⊃ φ₂ is realized by codes that transform
       realizers of φ₁ into realizers of φ₂
     - Universal quantification requires UNIFORM codes

  Connection to your research:

  - Traditional PCAs only support pure (non-effectful) computation
  - Evidenced Frames generalize this to support effects (state, non-determinism)
  - The key insight: abstract away from computational details,
    keep only the "evidence relation" φ₁ ⊢ φ₂
  - This lets you plug in different computational systems while
    maintaining the logical structure!
-}

-- Final summary test: All basic operations work
test-all-basic-operations :
  Σ (I-combinator apply K-code ≡ just K-code)
    (λ _ → Σ (K-code apply S-code ≡ just (App K-code S-code))
             (λ _ → church-true apply K-code ≡ just (App K-code K-code)))
test-all-basic-operations = refl , (refl , refl)
