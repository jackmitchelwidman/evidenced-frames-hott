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

module MAlonzo.Code.Everything where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.IO
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.PCA.PCA

import qualified Data.Text.IO as TIO
-- Everything.putStrLn
d_putStrLn_2 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.IO.T_IO_8
    () MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_putStrLn_2 = TIO.putStrLn
-- Everything.Code
d_Code_4 = ()
data T_Code_4
  = C_K'45'code_6 | C_S'45'code_8 | C_App_10 T_Code_4 T_Code_4
-- Everything.apply
d_apply_12 :: T_Code_4 -> T_Code_4 -> Maybe T_Code_4
d_apply_12 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    coe
      (case coe v0 of
         C_K'45'code_6
           -> coe
                MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                (coe C_App_10 (coe v0) (coe v1))
         C_S'45'code_8
           -> coe
                MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                (coe C_App_10 (coe v0) (coe v1))
         C_App_10 v3 v4
           -> case coe v3 of
                C_K'45'code_6
                  -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v4)
                C_S'45'code_8
                  -> coe
                       MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                       (coe C_App_10 (coe v0) (coe v1))
                C_App_10 v5 v6
                  -> case coe v5 of
                       C_S'45'code_8
                         -> let v7 = d_apply_12 (coe v6) (coe v1) in
                            coe
                              (let v8 = d_apply_12 (coe v4) (coe v1) in
                               coe
                                 (case coe v7 of
                                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v9
                                      -> case coe v8 of
                                           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v10
                                             -> coe d_apply_12 (coe v9) (coe v10)
                                           _ -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                                    _ -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18))
                       _ -> coe v2
                _ -> MAlonzo.RTE.mazUnreachableError
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Everything.just-inj
d_just'45'inj_58 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_just'45'inj_58 = erased
-- Everything.trans
d_trans_68 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_trans_68 = erased
-- Everything.sym
d_sym_76 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_76 = erased
-- Everything.apply-s-lemma
d_apply'45's'45'lemma_92 ::
  T_Code_4 ->
  T_Code_4 ->
  T_Code_4 ->
  T_Code_4 ->
  T_Code_4 ->
  T_Code_4 ->
  T_Code_4 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_apply'45's'45'lemma_92 = erased
-- Everything.MY_PCA
d_MY_PCA_142 :: MAlonzo.Code.PCA.PCA.T_PCA_4
d_MY_PCA_142
  = coe
      MAlonzo.Code.PCA.PCA.C_constructor_92 d_apply_12
      (coe C_K'45'code_6) (coe C_S'45'code_8)
main = coe d_main_174
-- Everything.main
d_main_174 ::
  MAlonzo.Code.Agda.Builtin.IO.T_IO_8
    AgdaAny MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_main_174 = coe d_putStrLn_2 ("Done" :: Data.Text.Text)
