module Basics.Nat where


data Nat : Set where
  zero : Nat
  suc  : Nat → Nat
