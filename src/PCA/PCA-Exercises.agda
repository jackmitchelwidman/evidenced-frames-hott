{-
  ════════════════════════════════════════════════════════════════════════════
  PCA EXERCISES - Start Here!
  ════════════════════════════════════════════════════════════════════════════

  This file contains exercises with holes {!!} for you to fill in.
  Work through them in order to understand PCAs deeply.
-}

module PCA.PCA-Exercises where

open import Agda.Builtin.Maybe
open import Agda.Builtin.Equality

{-
  ════════════════════════════════════════════════════════════════════════════
  THE PCA RECORD (from the paper, Definition II.2)
  ════════════════════════════════════════════════════════════════════════════
-}

record PCA : Set₁ where
  field
    Carrier : Set
    _·_     : Carrier → Carrier → Maybe Carrier
    k       : Carrier
    s       : Carrier

    -- The K law: K x y = x
    -- In our partial setting, we express this as:
    -- "If k·x terminates with k', then k'·y terminates with x"
    law-k   : ∀ {x y k'} → k · x ≡ just k' → k' · y ≡ just x

    -- The S law: S x y z = (x z)(y z)
    -- This is more complex because we need all intermediate steps to terminate
    law-s   : ∀ {x y z s₁ s₂ s₃ xz yz r} →
              s · x ≡ just s₁ →
              s₁ · y ≡ just s₂ →
              s₂ · z ≡ just s₃ →
              x · z ≡ just xz →
              y · z ≡ just yz →
              xz · yz ≡ just r →
              s₃ ≡ r

  -- Helper: lift application to Maybe
  infixl 7 _·?_
  _·?_ : Maybe Carrier → Maybe Carrier → Maybe Carrier
  nothing ·? _ = nothing
  _ ·? nothing = nothing
  (just f) ·? (just x) = f · x

{-
  ════════════════════════════════════════════════════════════════════════════
  EXERCISE 1: Identity Combinator
  ════════════════════════════════════════════════════════════════════════════

  The identity combinator I satisfies: I x = x

  In combinatory logic: I = S K K

  Why? Let's trace it:
      S K K x
    = K x (K x)    [by S law: S a b c = (a c)(b c)]
    = x            [by K law: K x y = x]

  Your task: Build I from S and K in a PCA.
-}

module Exercise1 (pca : PCA) where
  open PCA pca

  {-
    EXERCISE 1a: Define the identity combinator

    Hint: You need to:
    1. Apply s to k
    2. Apply the result to k again
    3. Handle the Maybe at each step
  -}

  i : Maybe Carrier
  i = {!!}

  -- EXERCISE 1b: Show that i behaves like identity (challenge!)
  -- i-identity : ∀ {x} → i ·? just x ≡ just x
  -- i-identity = {!!}

{-
  ════════════════════════════════════════════════════════════════════════════
  EXERCISE 2: Church Booleans
  ════════════════════════════════════════════════════════════════════════════

  Church encoding of booleans:
    true  = λt.λf. t    (returns first argument)
    false = λt.λf. f    (returns second argument)

  Notice:
    - true is just K!
    - false is K I (K applied to identity)
-}

module Exercise2 (pca : PCA) where
  open PCA pca
  open Exercise1 pca

  {-
    EXERCISE 2a: Church true

    This is easy - it's just k!
  -}

  church-true : Carrier
  church-true = {!!}

  {-
    EXERCISE 2b: Church false

    Hint: false = K I
    You have k : Carrier and i : Maybe Carrier
    How do you combine them?
  -}

  church-false : Maybe Carrier
  church-false = {!!}

  {-
    EXERCISE 2c: Boolean AND (challenge!)

    and = λa.λb. a b false

    Explanation:
    - If a is true:  true b false = b
    - If a is false: false b false = false

    Can you build this combinator?
  -}

  -- church-and : Carrier → Carrier → Maybe Carrier
  -- church-and = {!!}

{-
  ════════════════════════════════════════════════════════════════════════════
  EXERCISE 3: Church Numerals (from the paper, page 3)
  ════════════════════════════════════════════════════════════════════════════

  Church numerals represent ℕ as iteration:
    0 = λf.λx. x         (apply f zero times)
    1 = λf.λx. f x       (apply f once)
    2 = λf.λx. f (f x)   (apply f twice)
    n = λf.λx. f^n x     (apply f n times)

  From the paper:
    c^λ_0 = λf.λx. x = K I
    c^λ_(n+1) = λf.λx. f ((c^λ_n f) x)
-}

module Exercise3 (pca : PCA) where
  open PCA pca
  open Exercise1 pca

  {-
    EXERCISE 3a: Church numeral for zero

    Zero = λf.λx. x = K I

    This is the same as false!
  -}

  church-zero : Maybe Carrier
  church-zero = {!!}

  {-
    EXERCISE 3b: Church numeral for one

    One = λf.λx. f x

    In combinatory logic, this is called the "I*" combinator.
    One encoding: λf.λx. f x = S (K (S I)) K

    But that's complex! For now, just think about what it should do.
  -}

  -- This is advanced - don't worry if you can't solve it yet
  -- church-one : Maybe Carrier
  -- church-one = {!!}

{-
  ════════════════════════════════════════════════════════════════════════════
  EXERCISE 4: A Concrete PCA
  ════════════════════════════════════════════════════════════════════════════

  Let's build a real PCA to make everything concrete!
-}

module Exercise4 where

  data ℕ : Set where
    zero : ℕ
    suc  : ℕ → ℕ

  -- Simple lambda terms with S and K
  data Code : Set where
    K-code : Code
    S-code : Code
    App    : Code → Code → Code

  {-
    EXERCISE 4a: Define reduction

    We need a function that reduces lambda terms.
    Use the ℕ parameter to count steps (prevents infinite loops).

    Reduction rules:
    - K x y → x
    - S x y z → (x z)(y z)
  -}

  reduce : ℕ → Code → Maybe Code
  reduce zero c = nothing  -- out of steps
  reduce (suc n) K-code = just K-code
  reduce (suc n) S-code = just S-code
  reduce (suc n) (App K-code x) = {!!}  -- K x is already a value
  reduce (suc n) (App (App K-code x) y) = {!!}  -- K x y → x
  reduce (suc n) (App S-code x) = {!!}  -- S x is a value
  reduce (suc n) (App (App S-code x) y) = {!!}  -- S x y is a value
  reduce (suc n) (App (App (App S-code x) y) z) = {!!}  -- S x y z → ?
  reduce (suc n) (App f x) = {!!}  -- First reduce f, then apply to x

  {-
    EXERCISE 4b: Build a PCA from Code

    Show that our Code type with reduction forms a PCA!
  -}

  -- concrete-pca : PCA
  -- concrete-pca = record
  --   { Carrier = Code
  --   ; _·_ = λ f x → reduce 100 (App f x)
  --   ; k = K-code
  --   ; s = S-code
  --   ; law-k = {!!}
  --   ; law-s = {!!}
  --   }

{-
  ════════════════════════════════════════════════════════════════════════════
  NEXT STEPS
  ════════════════════════════════════════════════════════════════════════════

  After completing these exercises, you should understand:

  1. What a PCA is and why it's "functionally complete"
  2. How Church encodings represent data as functions
  3. How to build combinators from S and K
  4. How PCAs connect to computation

  The next file will show you how PCAs become evidenced frames!
  See: EvidenceFrames/FromPCA.agda (to be created)
-}
