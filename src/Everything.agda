{-# OPTIONS --guardedness #-}
{-# OPTIONS --no-positivity-check #-}


-- This file imports all modules in the project
-- Load this file with C-c C-l to type-check everything

module Everything where

open import Agda.Builtin.IO
open import Agda.Builtin.Unit
open import Agda.Builtin.String
open import Agda.Builtin.Maybe

-- PCA modules (working)
import PCA.PCA
import PCA.PCA-Tutorial
import PCA.PCAUnitTests
-- import PCA.PCA-Exercises  -- Has holes, uncomment when working on exercises
-- import PCA.PCA-Solutions  -- Not yet fixed

-- Basics (have minimal content)
-- open import Basics.Maybe
-- import Basics.Nat
import Basics.Utils

-- Realizability (have minimal content)
import Realizability.Predicates
import Realizability.RealizabilityLemmas
import Realizability.RealizabilityModel

-- Evidence Frames (empty - need implementation)
-- import EvidenceFrames.EvidenceFrame
-- import EvidenceFrames.EvidenceOperations
-- import EvidenceFrames.Examples

-- Tripos (empty - need implementation)
-- import Tripos.LogicalStructures
-- import Tripos.Quantifiers
-- import Tripos.Tripos

-- Topos (empty - need implementation)
-- import Topos.Category
-- import Topos.ToposProperties

-- Tests
-- import PCA.PCAUnitTests  -- Uncomment if this compiles


postulate putStrLn : String → IO ⊤ 

{-# FOREIGN GHC import qualified Data.Text.IO as TIO #-}
{-# COMPILE GHC putStrLn = TIO.putStrLn #-}

-- Define concrete codes
data Code : Set where
    K-code : Code
    S-code : Code
    App : Code → Code → Code

open import Agda.Builtin.Equality

{-# TERMINATING #-}
apply : Code → Code → Maybe Code
apply K-code x = just (App K-code x)
apply (App K-code x) y = just x
apply S-code x = just (App S-code x)
apply (App S-code x) y = just (App (App S-code x) y)
apply (App (App S-code x) y) z with apply x z | apply y z
... | just xz | just yz = apply xz yz
... | _ | _ = nothing
apply _ _ = nothing

just-inj : {A : Set} {x y : A} → just x ≡ just y → x ≡ y
just-inj refl = refl

trans : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

-- Helper lemma
apply-s-lemma : ∀ x y z xz yz s₃ r →
                apply (App (App S-code x) y) z ≡ just s₃ →
                apply x z ≡ just xz →
                apply y z ≡ just yz →
                apply xz yz ≡ just r →
                s₃ ≡ r
apply-s-lemma x y z xz yz s₃ r eq3 eq4 eq5 eq6
  with apply x z | apply y z
apply-s-lemma x y z xz yz s₃ r eq3 refl refl eq6
  | just xz | just yz
  rewrite eq6 = sym (just-inj eq3)

MY_PCA : PCA.PCA.PCA
MY_PCA = record
  { Carrier = Code
  ; _·_ = apply
  ; k = K-code
  ; s = S-code
  ; law-k = λ { {x} {y} refl → refl }
  ; law-s = λ { {x} {y} {z} {s₃ = s₃} {xz = xz} {yz = yz} {r = r}
                refl refl eq3 eq4 eq5 eq6 →
                apply-s-lemma x y z xz yz s₃ r eq3 eq4 eq5 eq6 }
  }




main : IO ⊤
main = putStrLn "Done"
