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

module MAlonzo.Code.PCA.PCAZ45ZTutorial where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Maybe

-- PCA.PCA-Tutorial.PCA
d_PCA_4 = ()
data T_PCA_4
  = C_constructor_84 (AgdaAny -> AgdaAny -> Maybe AgdaAny) AgdaAny
                     AgdaAny
-- PCA.PCA-Tutorial.PCA.Carrier
d_Carrier_42 :: T_PCA_4 -> ()
d_Carrier_42 = erased
-- PCA.PCA-Tutorial.PCA._·_
d__'183'__44 :: T_PCA_4 -> AgdaAny -> AgdaAny -> Maybe AgdaAny
d__'183'__44 v0
  = case coe v0 of
      C_constructor_84 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCA-Tutorial.PCA.k
d_k_46 :: T_PCA_4 -> AgdaAny
d_k_46 v0
  = case coe v0 of
      C_constructor_84 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCA-Tutorial.PCA.s
d_s_48 :: T_PCA_4 -> AgdaAny
d_s_48 v0
  = case coe v0 of
      C_constructor_84 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCA-Tutorial.PCA.law-k
d_law'45'k_56 ::
  T_PCA_4 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_law'45'k_56 = erased
-- PCA.PCA-Tutorial.PCA.law-s
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
-- PCA.PCA-Tutorial.PCA._·?_
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
-- PCA.PCA-Tutorial.ChurchEncodings._._·_
d__'183'__92 :: T_PCA_4 -> AgdaAny -> AgdaAny -> Maybe AgdaAny
d__'183'__92 v0 = coe d__'183'__44 (coe v0)
-- PCA.PCA-Tutorial.ChurchEncodings._._·?_
d__'183''63'__94 ::
  T_PCA_4 -> Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny
d__'183''63'__94 v0 = coe d__'183''63'__78 (coe v0)
-- PCA.PCA-Tutorial.ChurchEncodings._.Carrier
d_Carrier_96 :: T_PCA_4 -> ()
d_Carrier_96 = erased
-- PCA.PCA-Tutorial.ChurchEncodings._.k
d_k_98 :: T_PCA_4 -> AgdaAny
d_k_98 v0 = coe d_k_46 (coe v0)
-- PCA.PCA-Tutorial.ChurchEncodings._.law-k
d_law'45'k_100 ::
  T_PCA_4 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_law'45'k_100 = erased
-- PCA.PCA-Tutorial.ChurchEncodings._.law-s
d_law'45's_102 ::
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
d_law'45's_102 = erased
-- PCA.PCA-Tutorial.ChurchEncodings._.s
d_s_104 :: T_PCA_4 -> AgdaAny
d_s_104 v0 = coe d_s_48 (coe v0)
-- PCA.PCA-Tutorial.ChurchEncodings.church-true
d_church'45'true_106 :: T_PCA_4 -> AgdaAny
d_church'45'true_106 v0 = coe d_k_46 (coe v0)
-- PCA.PCA-Tutorial.ChurchNumerals._._·_
d__'183'__114 :: T_PCA_4 -> AgdaAny -> AgdaAny -> Maybe AgdaAny
d__'183'__114 v0 = coe d__'183'__44 (coe v0)
-- PCA.PCA-Tutorial.ChurchNumerals._._·?_
d__'183''63'__116 ::
  T_PCA_4 -> Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny
d__'183''63'__116 v0 = coe d__'183''63'__78 (coe v0)
-- PCA.PCA-Tutorial.ChurchNumerals._.Carrier
d_Carrier_118 :: T_PCA_4 -> ()
d_Carrier_118 = erased
-- PCA.PCA-Tutorial.ChurchNumerals._.k
d_k_120 :: T_PCA_4 -> AgdaAny
d_k_120 v0 = coe d_k_46 (coe v0)
-- PCA.PCA-Tutorial.ChurchNumerals._.law-k
d_law'45'k_122 ::
  T_PCA_4 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_law'45'k_122 = erased
-- PCA.PCA-Tutorial.ChurchNumerals._.law-s
d_law'45's_124 ::
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
d_law'45's_124 = erased
-- PCA.PCA-Tutorial.ChurchNumerals._.s
d_s_126 :: T_PCA_4 -> AgdaAny
d_s_126 v0 = coe d_s_48 (coe v0)
-- PCA.PCA-Tutorial.ConcretePCA.ℕ
d_ℕ_130 = ()
data T_ℕ_130 = C_zero_132 | C_suc_134 T_ℕ_130
-- PCA.PCA-Tutorial.ConcretePCA.Code
d_Code_136 = ()
data T_Code_136
  = C_K'8338'_138 | C_S'8338'_140 | C_App_142 T_Code_136 T_Code_136
-- PCA.PCA-Tutorial.ConcretePCA.reduce
d_reduce_144 :: T_ℕ_130 -> T_Code_136 -> Maybe T_Code_136
d_reduce_144 v0 v1
  = case coe v0 of
      C_zero_132 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      C_suc_134 v2
        -> case coe v1 of
             C_K'8338'_138
               -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v1)
             C_S'8338'_140
               -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v1)
             C_App_142 v3 v4
               -> let v5
                        = let v5 = d_reduce_144 (coe v2) (coe v3) in
                          coe
                            (case coe v5 of
                               MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v6
                                 -> coe d_reduce_144 (coe v2) (coe C_App_142 (coe v6) (coe v4))
                               MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v5
                               _ -> MAlonzo.RTE.mazUnreachableError) in
                  coe
                    (case coe v3 of
                       C_K'8338'_138
                         -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v1)
                       C_S'8338'_140
                         -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v1)
                       C_App_142 v6 v7
                         -> case coe v6 of
                              C_K'8338'_138
                                -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v7)
                              C_S'8338'_140
                                -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v1)
                              C_App_142 v8 v9
                                -> case coe v8 of
                                     C_S'8338'_140
                                       -> coe
                                            d_reduce_144 (coe v2)
                                            (coe
                                               C_App_142 (coe C_App_142 (coe v9) (coe v4))
                                               (coe C_App_142 (coe v7) (coe v4)))
                                     _ -> coe v5
                              _ -> MAlonzo.RTE.mazUnreachableError
                       _ -> MAlonzo.RTE.mazUnreachableError)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
