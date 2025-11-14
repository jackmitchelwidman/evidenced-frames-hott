module Basics.Maybe where


data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A
