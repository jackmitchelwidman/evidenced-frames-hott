{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module MAlonzo.Code.PCA.PCA where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Maybe

-- PCA.PCA.PCA
d_PCA_4 = ()
data T_PCA_4
  = C_constructor_92 (AgdaAny -> AgdaAny -> Maybe AgdaAny) AgdaAny
                     AgdaAny
-- PCA.PCA.PCA.Carrier
d_Carrier_42 :: T_PCA_4 -> ()
d_Carrier_42 = erased
-- PCA.PCA.PCA._·_
d__'183'__44 :: T_PCA_4 -> AgdaAny -> AgdaAny -> Maybe AgdaAny
d__'183'__44 v0
  = case coe v0 of
      C_constructor_92 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCA.PCA.k
d_k_46 :: T_PCA_4 -> AgdaAny
d_k_46 v0
  = case coe v0 of
      C_constructor_92 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCA.PCA.s
d_s_48 :: T_PCA_4 -> AgdaAny
d_s_48 v0
  = case coe v0 of
      C_constructor_92 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCA.PCA.law-k
d_law'45'k_56 ::
  T_PCA_4 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_law'45'k_56 = erased
-- PCA.PCA.PCA.law-s
d_law'45's_76 ::
  T_PCA_4 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_law'45's_76 = erased
-- PCA.PCA.PCA._·?_
d__'183''63'__78 ::
  T_PCA_4 -> Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny
d__'183''63'__78 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v4
               -> coe d__'183'__44 v0 v3 v4
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v2
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCA.PCA.i
d_i_84 :: T_PCA_4 -> Maybe AgdaAny
d_i_84 v0
  = let v1
          = coe d__'183'__44 v0 (d_s_48 (coe v0)) (d_k_46 (coe v0)) in
    coe
      (case coe v1 of
         MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v2
           -> coe d__'183'__44 v0 v2 (d_k_46 (coe v0))
         MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v1
         _ -> MAlonzo.RTE.mazUnreachableError)
