module PCA.PCA where

open import Agda.Builtin.Maybe
open import Agda.Builtin.Equality

record PCA : Set₁ where
  field
    Carrier : Set
    _·_     : Carrier → Carrier → Maybe Carrier
    k       : Carrier
    s       : Carrier

    -- K law: K x y = x
    law-k   : ∀ {x y k'} → k · x ≡ just k' → k' · y ≡ just x

    -- S law: S x y z = (x z)(y z)
    law-s   : ∀ {x y z s₁ s₂ s₃ xz yz r} →
              s · x ≡ just s₁ →
              s₁ · y ≡ just s₂ →
              s₂ · z ≡ just s₃ →
              x · z ≡ just xz →
              y · z ≡ just yz →
              xz · yz ≡ just r →
              s₃ ≡ r

  -- Helper operator for Maybe application
  infixl 7 _·?_

  _·?_ : Maybe Carrier → Maybe Carrier → Maybe Carrier
  nothing ·? _         = nothing
  _ ·? nothing         = nothing
  (just f) ·? (just x) = f · x

  -- Identity combinator: I = S K K
  i : Maybe Carrier
  i with s · k
  ... | nothing = nothing
  ... | just sk = sk · k
