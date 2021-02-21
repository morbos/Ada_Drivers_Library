--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.FLASH_KEY_STORE is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  .

   --  .
   type ACTIVATION_CODE_Registers is array (0 .. 297) of HAL.UInt32
     with Volatile;

   subtype SBKEY_HEADER1_TYPE_Field is HAL.UInt2;
   subtype SBKEY_HEADER1_INDEX_Field is HAL.UInt4;
   subtype SBKEY_HEADER1_SIZE_Field is HAL.UInt6;

   --  .
   type SBKEY_HEADER1_Register is record
      --  .
      TYPE_k         : SBKEY_HEADER1_TYPE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  .
      INDEX          : SBKEY_HEADER1_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  .
      SIZE           : SBKEY_HEADER1_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SBKEY_HEADER1_Register use record
      TYPE_k         at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      INDEX          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      SIZE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype USER_KEK_HEADER1_TYPE_Field is HAL.UInt2;
   subtype USER_KEK_HEADER1_INDEX_Field is HAL.UInt4;
   subtype USER_KEK_HEADER1_SIZE_Field is HAL.UInt6;

   --  .
   type USER_KEK_HEADER1_Register is record
      --  .
      TYPE_k         : USER_KEK_HEADER1_TYPE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  .
      INDEX          : USER_KEK_HEADER1_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  .
      SIZE           : USER_KEK_HEADER1_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USER_KEK_HEADER1_Register use record
      TYPE_k         at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      INDEX          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      SIZE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype UDS_HEADER1_TYPE_Field is HAL.UInt2;
   subtype UDS_HEADER1_INDEX_Field is HAL.UInt4;
   subtype UDS_HEADER1_SIZE_Field is HAL.UInt6;

   --  .
   type UDS_HEADER1_Register is record
      --  .
      TYPE_k         : UDS_HEADER1_TYPE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  .
      INDEX          : UDS_HEADER1_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  .
      SIZE           : UDS_HEADER1_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UDS_HEADER1_Register use record
      TYPE_k         at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      INDEX          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      SIZE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype PRINCE_REGION0_HEADER1_TYPE_Field is HAL.UInt2;
   subtype PRINCE_REGION0_HEADER1_INDEX_Field is HAL.UInt4;
   subtype PRINCE_REGION0_HEADER1_SIZE_Field is HAL.UInt6;

   --  .
   type PRINCE_REGION0_HEADER1_Register is record
      --  .
      TYPE_k         : PRINCE_REGION0_HEADER1_TYPE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  .
      INDEX          : PRINCE_REGION0_HEADER1_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  .
      SIZE           : PRINCE_REGION0_HEADER1_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRINCE_REGION0_HEADER1_Register use record
      TYPE_k         at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      INDEX          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      SIZE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype PRINCE_REGION1_HEADER1_TYPE_Field is HAL.UInt2;
   subtype PRINCE_REGION1_HEADER1_INDEX_Field is HAL.UInt4;
   subtype PRINCE_REGION1_HEADER1_SIZE_Field is HAL.UInt6;

   --  .
   type PRINCE_REGION1_HEADER1_Register is record
      --  .
      TYPE_k         : PRINCE_REGION1_HEADER1_TYPE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  .
      INDEX          : PRINCE_REGION1_HEADER1_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  .
      SIZE           : PRINCE_REGION1_HEADER1_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRINCE_REGION1_HEADER1_Register use record
      TYPE_k         at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      INDEX          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      SIZE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype PRINCE_REGION2_HEADER1_TYPE_Field is HAL.UInt2;
   subtype PRINCE_REGION2_HEADER1_INDEX_Field is HAL.UInt4;
   subtype PRINCE_REGION2_HEADER1_SIZE_Field is HAL.UInt6;

   --  .
   type PRINCE_REGION2_HEADER1_Register is record
      --  .
      TYPE_k         : PRINCE_REGION2_HEADER1_TYPE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  .
      INDEX          : PRINCE_REGION2_HEADER1_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  .
      SIZE           : PRINCE_REGION2_HEADER1_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRINCE_REGION2_HEADER1_Register use record
      TYPE_k         at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      INDEX          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      SIZE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type FLASH_KEY_STORE_Disc is
     (
      Header0,
      Key_Code0,
      Header1,
      Key_Code1,
      Body0,
      Key_Code2,
      Body1,
      Key_Code3,
      Body2,
      Key_Code4,
      Body3,
      Key_Code5,
      Body4,
      Key_Code6,
      Body5,
      Key_Code7,
      Body6,
      Key_Code8,
      Body7,
      Key_Code9,
      Body8,
      Key_Code10,
      Body9,
      Key_Code11,
      Body10,
      Key_Code12,
      Body11,
      Key_Code13);

   --  FLASH_KEY_STORE
   type FLASH_KEY_STORE_Peripheral
     (Discriminent : FLASH_KEY_STORE_Disc := Header0)
   is record
      --  Valid Key Sore Header : 0x95959595
      HEADER                    : aliased HAL.UInt32;
      --  puf discharge time in ms.
      puf_discharge_time_in_ms  : aliased HAL.UInt32;
      --  .
      ACTIVATION_CODE           : aliased ACTIVATION_CODE_Registers;
      case Discriminent is
         when Header0 =>
            --  .
            SBKEY_HEADER0 : aliased HAL.UInt32;
            --  .
            USER_KEK_HEADER0 : aliased HAL.UInt32;
            --  .
            UDS_HEADER0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_HEADER0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_HEADER0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_HEADER0 : aliased HAL.UInt32;
         when Key_Code0 =>
            --  .
            SBKEY_KEY_CODE0 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE0 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE0 : aliased HAL.UInt32;
         when Header1 =>
            --  .
            SBKEY_HEADER1 : aliased SBKEY_HEADER1_Register;
            --  .
            USER_KEK_HEADER1 : aliased USER_KEK_HEADER1_Register;
            --  .
            UDS_HEADER1 : aliased UDS_HEADER1_Register;
            --  .
            PRINCE_REGION0_HEADER1 : aliased PRINCE_REGION0_HEADER1_Register;
            --  .
            PRINCE_REGION1_HEADER1 : aliased PRINCE_REGION1_HEADER1_Register;
            --  .
            PRINCE_REGION2_HEADER1 : aliased PRINCE_REGION2_HEADER1_Register;
         when Key_Code1 =>
            --  .
            SBKEY_KEY_CODE1 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE1 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE1 : aliased HAL.UInt32;
         when Body0 =>
            --  .
            SBKEY_BODY0 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY0 : aliased HAL.UInt32;
            --  .
            UDS_BODY0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY0 : aliased HAL.UInt32;
         when Key_Code2 =>
            --  .
            SBKEY_KEY_CODE2 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE2 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE2 : aliased HAL.UInt32;
         when Body1 =>
            --  .
            SBKEY_BODY1 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY1 : aliased HAL.UInt32;
            --  .
            UDS_BODY1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY1 : aliased HAL.UInt32;
         when Key_Code3 =>
            --  .
            SBKEY_KEY_CODE3 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE3 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE3 : aliased HAL.UInt32;
         when Body2 =>
            --  .
            SBKEY_BODY2 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY2 : aliased HAL.UInt32;
            --  .
            UDS_BODY2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY2 : aliased HAL.UInt32;
         when Key_Code4 =>
            --  .
            SBKEY_KEY_CODE4 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE4 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE4 : aliased HAL.UInt32;
         when Body3 =>
            --  .
            SBKEY_BODY3 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY3 : aliased HAL.UInt32;
            --  .
            UDS_BODY3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY3 : aliased HAL.UInt32;
         when Key_Code5 =>
            --  .
            SBKEY_KEY_CODE5 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE5 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE5 : aliased HAL.UInt32;
         when Body4 =>
            --  .
            SBKEY_BODY4 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY4 : aliased HAL.UInt32;
            --  .
            UDS_BODY4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY4 : aliased HAL.UInt32;
         when Key_Code6 =>
            --  .
            SBKEY_KEY_CODE6 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE6 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE6 : aliased HAL.UInt32;
         when Body5 =>
            --  .
            SBKEY_BODY5 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY5 : aliased HAL.UInt32;
            --  .
            UDS_BODY5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY5 : aliased HAL.UInt32;
         when Key_Code7 =>
            --  .
            SBKEY_KEY_CODE7 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE7 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE7 : aliased HAL.UInt32;
         when Body6 =>
            --  .
            SBKEY_BODY6 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY6 : aliased HAL.UInt32;
            --  .
            UDS_BODY6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY6 : aliased HAL.UInt32;
         when Key_Code8 =>
            --  .
            SBKEY_KEY_CODE8 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE8 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE8 : aliased HAL.UInt32;
         when Body7 =>
            --  .
            SBKEY_BODY7 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY7 : aliased HAL.UInt32;
            --  .
            UDS_BODY7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY7 : aliased HAL.UInt32;
         when Key_Code9 =>
            --  .
            SBKEY_KEY_CODE9 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE9 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE9 : aliased HAL.UInt32;
         when Body8 =>
            --  .
            SBKEY_BODY8 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY8 : aliased HAL.UInt32;
            --  .
            UDS_BODY8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY8 : aliased HAL.UInt32;
         when Key_Code10 =>
            --  .
            SBKEY_KEY_CODE10 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE10 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE10 : aliased HAL.UInt32;
         when Body9 =>
            --  .
            SBKEY_BODY9 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY9 : aliased HAL.UInt32;
            --  .
            UDS_BODY9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY9 : aliased HAL.UInt32;
         when Key_Code11 =>
            --  .
            SBKEY_KEY_CODE11 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE11 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE11 : aliased HAL.UInt32;
         when Body10 =>
            --  .
            SBKEY_BODY10 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY10 : aliased HAL.UInt32;
            --  .
            UDS_BODY10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY10 : aliased HAL.UInt32;
         when Key_Code12 =>
            --  .
            SBKEY_KEY_CODE12 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE12 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE12 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE12 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE12 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE12 : aliased HAL.UInt32;
         when Body11 =>
            --  .
            SBKEY_BODY11 : aliased HAL.UInt32;
            --  .
            USER_KEK_BODY11 : aliased HAL.UInt32;
            --  .
            UDS_BODY11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_BODY11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_BODY11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_BODY11 : aliased HAL.UInt32;
         when Key_Code13 =>
            --  .
            SBKEY_KEY_CODE13 : aliased HAL.UInt32;
            --  .
            USER_KEK_KEY_CODE13 : aliased HAL.UInt32;
            --  .
            UDS_KEY_CODE13 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION0_KEY_CODE13 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_KEY_CODE13 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_KEY_CODE13 : aliased HAL.UInt32;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for FLASH_KEY_STORE_Peripheral use record
      HEADER                    at 16#0# range 0 .. 31;
      puf_discharge_time_in_ms  at 16#4# range 0 .. 31;
      ACTIVATION_CODE           at 16#8# range 0 .. 9535;
      SBKEY_HEADER0             at 16#4B0# range 0 .. 31;
      USER_KEK_HEADER0          at 16#4E8# range 0 .. 31;
      UDS_HEADER0               at 16#520# range 0 .. 31;
      PRINCE_REGION0_HEADER0    at 16#558# range 0 .. 31;
      PRINCE_REGION1_HEADER0    at 16#590# range 0 .. 31;
      PRINCE_REGION2_HEADER0    at 16#5C8# range 0 .. 31;
      SBKEY_KEY_CODE0           at 16#4B0# range 0 .. 31;
      USER_KEK_KEY_CODE0        at 16#4E8# range 0 .. 31;
      UDS_KEY_CODE0             at 16#520# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE0  at 16#558# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE0  at 16#590# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE0  at 16#5C8# range 0 .. 31;
      SBKEY_HEADER1             at 16#4B4# range 0 .. 31;
      USER_KEK_HEADER1          at 16#4EC# range 0 .. 31;
      UDS_HEADER1               at 16#524# range 0 .. 31;
      PRINCE_REGION0_HEADER1    at 16#55C# range 0 .. 31;
      PRINCE_REGION1_HEADER1    at 16#594# range 0 .. 31;
      PRINCE_REGION2_HEADER1    at 16#5CC# range 0 .. 31;
      SBKEY_KEY_CODE1           at 16#4B4# range 0 .. 31;
      USER_KEK_KEY_CODE1        at 16#4EC# range 0 .. 31;
      UDS_KEY_CODE1             at 16#524# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE1  at 16#55C# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE1  at 16#594# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE1  at 16#5CC# range 0 .. 31;
      SBKEY_BODY0               at 16#4B8# range 0 .. 31;
      USER_KEK_BODY0            at 16#4F0# range 0 .. 31;
      UDS_BODY0                 at 16#528# range 0 .. 31;
      PRINCE_REGION0_BODY0      at 16#560# range 0 .. 31;
      PRINCE_REGION1_BODY0      at 16#598# range 0 .. 31;
      PRINCE_REGION2_BODY0      at 16#5D0# range 0 .. 31;
      SBKEY_KEY_CODE2           at 16#4B8# range 0 .. 31;
      USER_KEK_KEY_CODE2        at 16#4F0# range 0 .. 31;
      UDS_KEY_CODE2             at 16#528# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE2  at 16#560# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE2  at 16#598# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE2  at 16#5D0# range 0 .. 31;
      SBKEY_BODY1               at 16#4BC# range 0 .. 31;
      USER_KEK_BODY1            at 16#4F4# range 0 .. 31;
      UDS_BODY1                 at 16#52C# range 0 .. 31;
      PRINCE_REGION0_BODY1      at 16#564# range 0 .. 31;
      PRINCE_REGION1_BODY1      at 16#59C# range 0 .. 31;
      PRINCE_REGION2_BODY1      at 16#5D4# range 0 .. 31;
      SBKEY_KEY_CODE3           at 16#4BC# range 0 .. 31;
      USER_KEK_KEY_CODE3        at 16#4F4# range 0 .. 31;
      UDS_KEY_CODE3             at 16#52C# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE3  at 16#564# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE3  at 16#59C# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE3  at 16#5D4# range 0 .. 31;
      SBKEY_BODY2               at 16#4C0# range 0 .. 31;
      USER_KEK_BODY2            at 16#4F8# range 0 .. 31;
      UDS_BODY2                 at 16#530# range 0 .. 31;
      PRINCE_REGION0_BODY2      at 16#568# range 0 .. 31;
      PRINCE_REGION1_BODY2      at 16#5A0# range 0 .. 31;
      PRINCE_REGION2_BODY2      at 16#5D8# range 0 .. 31;
      SBKEY_KEY_CODE4           at 16#4C0# range 0 .. 31;
      USER_KEK_KEY_CODE4        at 16#4F8# range 0 .. 31;
      UDS_KEY_CODE4             at 16#530# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE4  at 16#568# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE4  at 16#5A0# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE4  at 16#5D8# range 0 .. 31;
      SBKEY_BODY3               at 16#4C4# range 0 .. 31;
      USER_KEK_BODY3            at 16#4FC# range 0 .. 31;
      UDS_BODY3                 at 16#534# range 0 .. 31;
      PRINCE_REGION0_BODY3      at 16#56C# range 0 .. 31;
      PRINCE_REGION1_BODY3      at 16#5A4# range 0 .. 31;
      PRINCE_REGION2_BODY3      at 16#5DC# range 0 .. 31;
      SBKEY_KEY_CODE5           at 16#4C4# range 0 .. 31;
      USER_KEK_KEY_CODE5        at 16#4FC# range 0 .. 31;
      UDS_KEY_CODE5             at 16#534# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE5  at 16#56C# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE5  at 16#5A4# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE5  at 16#5DC# range 0 .. 31;
      SBKEY_BODY4               at 16#4C8# range 0 .. 31;
      USER_KEK_BODY4            at 16#500# range 0 .. 31;
      UDS_BODY4                 at 16#538# range 0 .. 31;
      PRINCE_REGION0_BODY4      at 16#570# range 0 .. 31;
      PRINCE_REGION1_BODY4      at 16#5A8# range 0 .. 31;
      PRINCE_REGION2_BODY4      at 16#5E0# range 0 .. 31;
      SBKEY_KEY_CODE6           at 16#4C8# range 0 .. 31;
      USER_KEK_KEY_CODE6        at 16#500# range 0 .. 31;
      UDS_KEY_CODE6             at 16#538# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE6  at 16#570# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE6  at 16#5A8# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE6  at 16#5E0# range 0 .. 31;
      SBKEY_BODY5               at 16#4CC# range 0 .. 31;
      USER_KEK_BODY5            at 16#504# range 0 .. 31;
      UDS_BODY5                 at 16#53C# range 0 .. 31;
      PRINCE_REGION0_BODY5      at 16#574# range 0 .. 31;
      PRINCE_REGION1_BODY5      at 16#5AC# range 0 .. 31;
      PRINCE_REGION2_BODY5      at 16#5E4# range 0 .. 31;
      SBKEY_KEY_CODE7           at 16#4CC# range 0 .. 31;
      USER_KEK_KEY_CODE7        at 16#504# range 0 .. 31;
      UDS_KEY_CODE7             at 16#53C# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE7  at 16#574# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE7  at 16#5AC# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE7  at 16#5E4# range 0 .. 31;
      SBKEY_BODY6               at 16#4D0# range 0 .. 31;
      USER_KEK_BODY6            at 16#508# range 0 .. 31;
      UDS_BODY6                 at 16#540# range 0 .. 31;
      PRINCE_REGION0_BODY6      at 16#578# range 0 .. 31;
      PRINCE_REGION1_BODY6      at 16#5B0# range 0 .. 31;
      PRINCE_REGION2_BODY6      at 16#5E8# range 0 .. 31;
      SBKEY_KEY_CODE8           at 16#4D0# range 0 .. 31;
      USER_KEK_KEY_CODE8        at 16#508# range 0 .. 31;
      UDS_KEY_CODE8             at 16#540# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE8  at 16#578# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE8  at 16#5B0# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE8  at 16#5E8# range 0 .. 31;
      SBKEY_BODY7               at 16#4D4# range 0 .. 31;
      USER_KEK_BODY7            at 16#50C# range 0 .. 31;
      UDS_BODY7                 at 16#544# range 0 .. 31;
      PRINCE_REGION0_BODY7      at 16#57C# range 0 .. 31;
      PRINCE_REGION1_BODY7      at 16#5B4# range 0 .. 31;
      PRINCE_REGION2_BODY7      at 16#5EC# range 0 .. 31;
      SBKEY_KEY_CODE9           at 16#4D4# range 0 .. 31;
      USER_KEK_KEY_CODE9        at 16#50C# range 0 .. 31;
      UDS_KEY_CODE9             at 16#544# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE9  at 16#57C# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE9  at 16#5B4# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE9  at 16#5EC# range 0 .. 31;
      SBKEY_BODY8               at 16#4D8# range 0 .. 31;
      USER_KEK_BODY8            at 16#510# range 0 .. 31;
      UDS_BODY8                 at 16#548# range 0 .. 31;
      PRINCE_REGION0_BODY8      at 16#580# range 0 .. 31;
      PRINCE_REGION1_BODY8      at 16#5B8# range 0 .. 31;
      PRINCE_REGION2_BODY8      at 16#5F0# range 0 .. 31;
      SBKEY_KEY_CODE10          at 16#4D8# range 0 .. 31;
      USER_KEK_KEY_CODE10       at 16#510# range 0 .. 31;
      UDS_KEY_CODE10            at 16#548# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE10 at 16#580# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE10 at 16#5B8# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE10 at 16#5F0# range 0 .. 31;
      SBKEY_BODY9               at 16#4DC# range 0 .. 31;
      USER_KEK_BODY9            at 16#514# range 0 .. 31;
      UDS_BODY9                 at 16#54C# range 0 .. 31;
      PRINCE_REGION0_BODY9      at 16#584# range 0 .. 31;
      PRINCE_REGION1_BODY9      at 16#5BC# range 0 .. 31;
      PRINCE_REGION2_BODY9      at 16#5F4# range 0 .. 31;
      SBKEY_KEY_CODE11          at 16#4DC# range 0 .. 31;
      USER_KEK_KEY_CODE11       at 16#514# range 0 .. 31;
      UDS_KEY_CODE11            at 16#54C# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE11 at 16#584# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE11 at 16#5BC# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE11 at 16#5F4# range 0 .. 31;
      SBKEY_BODY10              at 16#4E0# range 0 .. 31;
      USER_KEK_BODY10           at 16#518# range 0 .. 31;
      UDS_BODY10                at 16#550# range 0 .. 31;
      PRINCE_REGION0_BODY10     at 16#588# range 0 .. 31;
      PRINCE_REGION1_BODY10     at 16#5C0# range 0 .. 31;
      PRINCE_REGION2_BODY10     at 16#5F8# range 0 .. 31;
      SBKEY_KEY_CODE12          at 16#4E0# range 0 .. 31;
      USER_KEK_KEY_CODE12       at 16#518# range 0 .. 31;
      UDS_KEY_CODE12            at 16#550# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE12 at 16#588# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE12 at 16#5C0# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE12 at 16#5F8# range 0 .. 31;
      SBKEY_BODY11              at 16#4E4# range 0 .. 31;
      USER_KEK_BODY11           at 16#51C# range 0 .. 31;
      UDS_BODY11                at 16#554# range 0 .. 31;
      PRINCE_REGION0_BODY11     at 16#58C# range 0 .. 31;
      PRINCE_REGION1_BODY11     at 16#5C4# range 0 .. 31;
      PRINCE_REGION2_BODY11     at 16#5FC# range 0 .. 31;
      SBKEY_KEY_CODE13          at 16#4E4# range 0 .. 31;
      USER_KEK_KEY_CODE13       at 16#51C# range 0 .. 31;
      UDS_KEY_CODE13            at 16#554# range 0 .. 31;
      PRINCE_REGION0_KEY_CODE13 at 16#58C# range 0 .. 31;
      PRINCE_REGION1_KEY_CODE13 at 16#5C4# range 0 .. 31;
      PRINCE_REGION2_KEY_CODE13 at 16#5FC# range 0 .. 31;
   end record;

   --  FLASH_KEY_STORE
   FLASH_KEY_STORE_Periph : aliased FLASH_KEY_STORE_Peripheral
     with Import, Address => System'To_Address (16#9E600#);

end NXP_SVD.FLASH_KEY_STORE;
