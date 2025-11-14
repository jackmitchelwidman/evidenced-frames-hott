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

module MAlonzo.Code.PCA.PCAUnitTests where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit

-- PCA.PCAUnitTests.Code
d_Code_4 = ()
data T_Code_4
  = C_K'45'code_6 | C_S'45'code_8 | C_App_10 T_Code_4 T_Code_4
-- PCA.PCAUnitTests.case_of_
d_case_of__16 ::
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_case_of__16 ~v0 ~v1 v2 v3 = du_case_of__16 v2 v3
du_case_of__16 :: AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du_case_of__16 v0 v1 = coe v1 v0
-- PCA.PCAUnitTests.reduce
d_reduce_22 :: Integer -> T_Code_4 -> T_Code_4 -> Maybe T_Code_4
d_reduce_22 v0 v1 v2
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (case coe v1 of
                C_K'45'code_6
                  -> coe
                       MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                       (coe C_App_10 (coe v1) (coe v2))
                C_S'45'code_8
                  -> coe
                       MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                       (coe C_App_10 (coe v1) (coe v2))
                C_App_10 v4 v5
                  -> let v6
                           = coe
                               du_case_of__16 (coe d_reduce_22 (coe v3) (coe v4) (coe v5))
                               (coe du_'46'extendedlambda2_72 (coe v3) (coe v2)) in
                     coe
                       (case coe v4 of
                          C_K'45'code_6
                            -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v5)
                          C_S'45'code_8
                            -> coe
                                 MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                                 (coe C_App_10 (coe v1) (coe v2))
                          C_App_10 v7 v8
                            -> case coe v7 of
                                 C_S'45'code_8
                                   -> coe
                                        du_case_of__16 (coe d_reduce_22 (coe v3) (coe v8) (coe v2))
                                        (coe du_'46'extendedlambda0_56 (coe v3) (coe v5) (coe v2))
                                 _ -> coe v6
                          _ -> MAlonzo.RTE.mazUnreachableError)
                _ -> MAlonzo.RTE.mazUnreachableError)
-- PCA.PCAUnitTests..extendedlambda0
d_'46'extendedlambda0_56 ::
  Integer ->
  T_Code_4 ->
  T_Code_4 -> T_Code_4 -> Maybe T_Code_4 -> Maybe T_Code_4
d_'46'extendedlambda0_56 v0 ~v1 v2 v3 v4
  = du_'46'extendedlambda0_56 v0 v2 v3 v4
du_'46'extendedlambda0_56 ::
  Integer -> T_Code_4 -> T_Code_4 -> Maybe T_Code_4 -> Maybe T_Code_4
du_'46'extendedlambda0_56 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v4
        -> coe
             du_case_of__16 (coe d_reduce_22 (coe v0) (coe v1) (coe v2))
             (coe du_'46'extendedlambda1_60 (coe v0) (coe v4))
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCAUnitTests..extendedlambda1
d_'46'extendedlambda1_60 ::
  Integer ->
  T_Code_4 ->
  T_Code_4 ->
  T_Code_4 -> T_Code_4 -> Maybe T_Code_4 -> Maybe T_Code_4
d_'46'extendedlambda1_60 v0 ~v1 ~v2 ~v3 v4 v5
  = du_'46'extendedlambda1_60 v0 v4 v5
du_'46'extendedlambda1_60 ::
  Integer -> T_Code_4 -> Maybe T_Code_4 -> Maybe T_Code_4
du_'46'extendedlambda1_60 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3
        -> coe d_reduce_22 (coe v0) (coe v1) (coe v3)
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCAUnitTests..extendedlambda2
d_'46'extendedlambda2_72 ::
  Integer ->
  T_Code_4 ->
  T_Code_4 -> T_Code_4 -> Maybe T_Code_4 -> Maybe T_Code_4
d_'46'extendedlambda2_72 v0 ~v1 ~v2 v3 v4
  = du_'46'extendedlambda2_72 v0 v3 v4
du_'46'extendedlambda2_72 ::
  Integer -> T_Code_4 -> Maybe T_Code_4 -> Maybe T_Code_4
du_'46'extendedlambda2_72 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3
        -> coe d_reduce_22 (coe v0) (coe v3) (coe v1)
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- PCA.PCAUnitTests._apply_
d__apply__76 :: T_Code_4 -> T_Code_4 -> Maybe T_Code_4
d__apply__76 v0 v1
  = coe d_reduce_22 (coe (100 :: Integer)) (coe v0) (coe v1)
-- PCA.PCAUnitTests.test-k-law-1
d_test'45'k'45'law'45'1_82 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'k'45'law'45'1_82 = erased
-- PCA.PCAUnitTests.test-k-law-2
d_test'45'k'45'law'45'2_84 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'k'45'law'45'2_84 = erased
-- PCA.PCAUnitTests.test-k-law-3
d_test'45'k'45'law'45'3_86 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'k'45'law'45'3_86 = erased
-- PCA.PCAUnitTests.test-k-law-4
d_test'45'k'45'law'45'4_88 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'k'45'law'45'4_88 = erased
-- PCA.PCAUnitTests.I-combinator
d_I'45'combinator_90 :: T_Code_4
d_I'45'combinator_90
  = coe
      C_App_10 (coe C_App_10 (coe C_S'45'code_8) (coe C_K'45'code_6))
      (coe C_K'45'code_6)
-- PCA.PCAUnitTests.test-identity-K
d_test'45'identity'45'K_92 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'identity'45'K_92 = erased
-- PCA.PCAUnitTests.test-identity-S
d_test'45'identity'45'S_94 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'identity'45'S_94 = erased
-- PCA.PCAUnitTests.test-identity-complex
d_test'45'identity'45'complex_96 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'identity'45'complex_96 = erased
-- PCA.PCAUnitTests.church-true
d_church'45'true_98 :: T_Code_4
d_church'45'true_98 = coe C_K'45'code_6
-- PCA.PCAUnitTests.church-false
d_church'45'false_100 :: T_Code_4
d_church'45'false_100
  = coe C_App_10 (coe C_K'45'code_6) (coe d_I'45'combinator_90)
-- PCA.PCAUnitTests.test-church-true-1
d_test'45'church'45'true'45'1_102 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'church'45'true'45'1_102 = erased
-- PCA.PCAUnitTests.test-church-true-2
d_test'45'church'45'true'45'2_104 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'church'45'true'45'2_104 = erased
-- PCA.PCAUnitTests.test-church-false-1
d_test'45'church'45'false'45'1_106 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'church'45'false'45'1_106 = erased
-- PCA.PCAUnitTests.church-0
d_church'45'0_108 :: T_Code_4
d_church'45'0_108
  = coe C_App_10 (coe C_K'45'code_6) (coe d_I'45'combinator_90)
-- PCA.PCAUnitTests.church-1
d_church'45'1_110 :: T_Code_4
d_church'45'1_110 = coe d_I'45'combinator_90
-- PCA.PCAUnitTests.test-church-0
d_test'45'church'45'0_112 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'church'45'0_112 = erased
-- PCA.PCAUnitTests.test-church-1
d_test'45'church'45'1_114 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'church'45'1_114 = erased
-- PCA.PCAUnitTests.test-uniformity-1
d_test'45'uniformity'45'1_116 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'uniformity'45'1_116 = erased
-- PCA.PCAUnitTests.test-uniformity-2
d_test'45'uniformity'45'2_118 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'uniformity'45'2_118 = erased
-- PCA.PCAUnitTests.test-uniformity-3
d_test'45'uniformity'45'3_120 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'uniformity'45'3_120 = erased
-- PCA.PCAUnitTests.is-K-code
d_is'45'K'45'code_122 :: T_Code_4 -> ()
d_is'45'K'45'code_122 = erased
-- PCA.PCAUnitTests._.⊥
d_'8869'_128 a0 = ()
data T_'8869'_128
-- PCA.PCAUnitTests.evidence-identity-preserves-K
d_evidence'45'identity'45'preserves'45'K_136 ::
  T_Code_4 -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_evidence'45'identity'45'preserves'45'K_136 v0 v1
  = coe
      seq (coe v0)
      (coe
         seq (coe v1)
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe C_K'45'code_6)
            (coe
               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased
               (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))))
-- PCA.PCAUnitTests.B-combinator
d_B'45'combinator_142 :: T_Code_4
d_B'45'combinator_142
  = coe
      C_App_10
      (coe
         C_App_10 (coe C_S'45'code_8)
         (coe C_App_10 (coe C_K'45'code_6) (coe C_S'45'code_8)))
      (coe C_K'45'code_6)
-- PCA.PCAUnitTests.test-composition
d_test'45'composition_158 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_test'45'composition_158 = erased
-- PCA.PCAUnitTests.test-all-basic-operations
d_test'45'all'45'basic'45'operations_164 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_test'45'all'45'basic'45'operations_164
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased)
