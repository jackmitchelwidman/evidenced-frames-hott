{-
  ════════════════════════════════════════════════════════════════════════════
  PARTIAL COMBINATORY ALGEBRAS (PCAs) - Interactive Tutorial
  ════════════════════════════════════════════════════════════════════════════

  This file is designed to teach you about PCAs through interactive exercises.

  What is a PCA?
  ──────────────
  A Partial Combinatory Algebra is a mathematical structure that captures
  the essence of computation. It consists of:

  1. A set C of "codes" (think: programs or λ-terms)
  2. A partial application operation · : C × C ⇀ C
  3. Two special elements K and S that satisfy certain laws

  Why "Partial"?
  ──────────────
  The application operation is PARTIAL because not all computations terminate.
  For example, (λx. x x)(λx. x x) runs forever and never produces a result.
  We use Maybe to represent this: nothing = diverges, just c = terminates with c

  Why K and S?
  ─────────────
  These are the S and K combinators from combinatory logic. They are special
  because ANY λ-term can be encoded using only S, K, and application!

  K is the "constant" combinator:     K x y = x
  S is the "substitution" combinator: S x y z = (x z) (y z)

  This means PCAs are "functionally complete" - they can express all computable
  functions, just like the λ-calculus or Turing machines.

  Connection to Realizability:
  ────────────────────────────
  In realizability, a proposition is "true" if we can compute a witness for it.
  PCAs provide the computational system that produces these witnesses.
  The codes in a PCA become the "realizers" in realizability models.

  ════════════════════════════════════════════════════════════════════════════
-}

module PCA.PCA-Tutorial where

open import Agda.Builtin.Maybe
open import Agda.Builtin.Equality
open import Agda.Builtin.Bool

{-
  ════════════════════════════════════════════════════════════════════════════
  PART 1: The PCA Record
  ════════════════════════════════════════════════════════════════════════════
-}

record PCA : Set₁ where
  field
    -- The set of "codes" or "programs"
    Carrier : Set

    -- Partial application: may diverge (return nothing)
    _·_     : Carrier → Carrier → Maybe Carrier

    -- The K combinator: K x y = x (returns first argument, ignores second)
    k       : Carrier

    -- The S combinator: S x y z = (x z)(y z) (distributes application)
    s       : Carrier

    -- K must satisfy its law: K x y must terminate and equal x
    law-k   : ∀ {x y k'} → k · x ≡ just k' → k' · y ≡ just x

    -- S must satisfy its law: S x y z must do the same as (x z)(y z)
    law-s   : ∀ {x y z s₁ s₂ s₃ xz yz r} →
              s · x ≡ just s₁ →
              s₁ · y ≡ just s₂ →
              s₂ · z ≡ just s₃ →
              x · z ≡ just xz →
              y · z ≡ just yz →
              xz · yz ≡ just r →
              s₃ ≡ r

  {-
    Helper function: Lift application to Maybe values
    This lets us write: (just k) ·? (just x) instead of k · x
  -}
  infixl 7 _·?_
  _·?_ : Maybe Carrier → Maybe Carrier → Maybe Carrier
  nothing ·? _         = nothing
  _ ·? nothing         = nothing
  (just f) ·? (just x) = f · x

  {-
    ──────────────────────────────────────────────────────────────────────────
    EXERCISE 1: Define the Identity Combinator
    ──────────────────────────────────────────────────────────────────────────

    The I combinator is the identity function: I x = x

    In combinatory logic, we can define I using S and K:
        I = S K K

    Why does this work?
        S K K x
      = K x (K x)      [by S law]
      = x              [by K law]

    Your task: Define I using s and k, and prove it satisfies the identity law.

    Hint: You'll need to apply s to k twice, handling the Maybe at each step.
  -}

  -- Uncomment and complete:
  -- i : Maybe Carrier
  -- i = {!!}  -- Hint: Start with s · k, then apply the result to k again

  -- Once you define i, try to prove this property:
  -- i-law : ∀ {x} → i ·? just x ≡ just x
  -- i-law = {!!}


{-
  ════════════════════════════════════════════════════════════════════════════
  PART 2: Church Encodings in PCAs
  ════════════════════════════════════════════════════════════════════════════

  Church encodings represent data (like booleans, natural numbers, pairs)
  as pure functions. This is important because PCAs only have codes and
  application - no built-in data types!

  In realizability, Church encodings connect mathematical objects (like ℕ)
  to their computational representations in the PCA.
-}

module ChurchEncodings (pca : PCA) where
  open PCA pca

  {-
    ──────────────────────────────────────────────────────────────────────────
    Church Booleans
    ──────────────────────────────────────────────────────────────────────────

    A boolean is a function that takes two arguments and picks one:
        true  = λt.λf.t    (picks the first argument)
        false = λt.λf.f    (picks the second argument)

    Notice: true is just K! (K x y = x)

    What about false?
        false = λt.λf.f = λt.(λf.f) = λt.(K I) = K I

    where I is the identity combinator.
  -}

  -- EXERCISE 2: Define Church encodings for true and false

  -- true should return first argument, ignore second (that's K!)
  church-true : Carrier
  church-true = k

  -- EXERCISE: Define church-false
  -- Hint: You need K I, which is k applied to i
  -- But you need to extract i from Maybe first...
  -- church-false : Maybe Carrier
  -- church-false = {!!}

  {-
    ──────────────────────────────────────────────────────────────────────────
    Church Pairs
    ──────────────────────────────────────────────────────────────────────────

    A pair is a function that takes a selector and applies it to both elements:
        pair x y = λs. s x y

    To extract elements:
        fst p = p true  = p (λx.λy.x) = (λs. s x y)(λx.λy.x) = (λx.λy.x) x y = x
        snd p = p false = p (λx.λy.y) = (λs. s x y)(λx.λy.y) = (λx.λy.y) x y = y
  -}

  -- EXERCISE 3: Define a pairing combinator
  -- Hint: pair x y = λs. s x y
  --       In combinatory logic: pair = λx.λy.λs. s x y = S (S (K S) K)
  -- But let's use a simpler encoding for now...

  -- Try to think through: what combinator takes x, y, and a function f,
  -- and returns f x y?


{-
  ════════════════════════════════════════════════════════════════════════════
  PART 3: Natural Numbers in PCAs (Church Numerals)
  ════════════════════════════════════════════════════════════════════════════

  Church numerals represent numbers as iteration:
      0 = λf.λx.x              (apply f zero times)
      1 = λf.λx.f x            (apply f once)
      2 = λf.λx.f (f x)        (apply f twice)
      n = λf.λx.f^n x          (apply f n times)

  This connects to the paper's definition on page 3:
      c_λ^0 = λf.λx.x = K I
      c_λ^(n+1) = λf.λx. f ((c_λ^n f) x)
-}

module ChurchNumerals (pca : PCA) where
  open PCA pca

  -- EXERCISE 4: Define Church numeral for zero
  -- Hint: 0 = λf.λx.x = K I (ignores f, returns x)
  -- church-zero : Maybe Carrier
  -- church-zero = {!!}

  {-
    The successor function: given a numeral n, produce n+1

    In λ-calculus:
        succ = λn.λf.λx. f (n f x)

    Explanation:
        n f x = f applied n times to x
        f (n f x) = f applied n+1 times to x

    This is tricky to build from S and K alone!
  -}

  -- We'll come back to this after learning more...


{-
  ════════════════════════════════════════════════════════════════════════════
  PART 4: Why PCAs Matter for Realizability
  ════════════════════════════════════════════════════════════════════════════

  From the paper (Section II-C):

  "A code c realizes that predicate φ₁ entails φ₂ when, given any realizer
   of φ₁(i), the code c can convert it into a realizer of φ₂(i)."

  Key insight: UNIFORMITY
  ──────────────────────────
  The SAME code c must work for ALL elements i. This ensures entailment
  is computational - you can't use a different code for each i.

  Example from the paper:
  ────────────────────────
  Let n̄ be the predicate on ℕ where n̄(n) is realized by the Church numeral c_λ^n.

  For a function f : ℕ → ℕ, define φ_f where φ_f(n) is realized by c_λ^(f(n)).

  Question: When does n̄ entail φ_f?

  Answer: If and only if f is COMPUTABLE according to the PCA!

  Why? Because we need a SINGLE code that converts c_λ^n into c_λ^(f(n))
  for ALL n. Such a code exists iff f is computable.

  This is the heart of realizability: logical entailment = computability!
-}

{-
  ════════════════════════════════════════════════════════════════════════════
  PART 5: Building a Concrete PCA
  ════════════════════════════════════════════════════════════════════════════

  Let's build a simple concrete PCA to make everything concrete.
  We'll use a small language with λ-terms as codes.
-}

module ConcretePCA where

  -- Helper for natural numbers
  data ℕ : Set where
    zero : ℕ
    suc  : ℕ → ℕ

  -- A simple λ-calculus with explicit S and K
  data Code : Set where
    Kₒ  : Code                    -- K combinator
    Sₒ  : Code                    -- S combinator
    App : Code → Code → Code      -- Application

  -- Call-by-name reduction (may not terminate, hence ℕ → Maybe Code)
  -- We'll use a step counter to ensure termination of the evaluator itself

  reduce : ℕ → Code → Maybe Code
  reduce zero c = nothing  -- out of steps = divergence
  reduce (suc n) Kₒ = just Kₒ
  reduce (suc n) Sₒ = just Sₒ
  reduce (suc n) (App Kₒ x) = just (App Kₒ x)  -- K x is a value
  reduce (suc n) (App (App Kₒ x) y) = just x   -- K x y → x
  reduce (suc n) (App Sₒ x) = just (App Sₒ x)  -- S x is a value
  reduce (suc n) (App (App Sₒ x) y) = just (App (App Sₒ x) y)  -- S x y is a value
  reduce (suc n) (App (App (App Sₒ x) y) z) =
    -- S x y z → x z (y z)
    reduce n (App (App x z) (App y z))
  reduce (suc n) (App f x) with reduce n f
  ... | nothing = nothing
  ... | (just f′) = reduce n (App f′ x)

  -- EXERCISE 5: Complete the PCA instance for our concrete Code
  -- This shows that our simple calculus IS a PCA!

  -- concrete-pca : PCA
  -- concrete-pca = record
  --   { Carrier = Code
  --   ; _·_ = λ f x → {!!}  -- Hint: reduce (suc 100) (App f x)
  --   ; k = Kₒ
  --   ; s = Sₒ
  --   ; law-k = {!!}  -- You'll need to prove this by computation
  --   ; law-s = {!!}  -- You'll need to prove this by computation
  --   }


{-
  ════════════════════════════════════════════════════════════════════════════
  REFLECTION QUESTIONS
  ════════════════════════════════════════════════════════════════════════════

  After working through these exercises, consider:

  1. Why is partiality essential for PCAs?
     (What would happen if all applications terminated?)

  2. Why are S and K sufficient to encode all computations?
     (This is called "functional completeness")

  3. How do Church encodings connect mathematical objects to computations?

  4. Why is uniformity crucial for realizability?
     (What would break if we allowed different codes for different indices?)

  5. How does this relate to evidenced frames?
     (Hint: PCAs provide the "evidence" E in an evidenced frame!)

  ════════════════════════════════════════════════════════════════════════════
-}
