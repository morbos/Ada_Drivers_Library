--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.AHB_SECURE_CTRL is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Security access rules for the whole FLASH : 0x0000_0000 - 0x0009_FFFF
   type SEC_CTRL_FLASH_ROM_SLAVE_RULE_FLASH_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_ROM_SLAVE_RULE_FLASH_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for the whole ROM : 0x0300_0000 - 0x0301_FFFF
   type SEC_CTRL_FLASH_ROM_SLAVE_RULE_ROM_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_ROM_SLAVE_RULE_ROM_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for Flash and ROM slaves.
   type SEC_CTRL_FLASH_ROM_SLAVE_RULE_Register is record
      --  Security access rules for the whole FLASH : 0x0000_0000 - 0x0009_FFFF
      FLASH_RULE    : SEC_CTRL_FLASH_ROM_SLAVE_RULE_FLASH_RULE_Field :=
                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3  : HAL.UInt2 := 16#0#;
      --  Security access rules for the whole ROM : 0x0300_0000 - 0x0301_FFFF
      ROM_RULE      : SEC_CTRL_FLASH_ROM_SLAVE_RULE_ROM_RULE_Field :=
                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_FLASH_ROM_SLAVE_RULE_Register use record
      FLASH_RULE    at 0 range 0 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      ROM_RULE      at 0 range 4 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  secure control rule0. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_FLASH_MEM_RULE0_RULE0_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_MEM_RULE0_RULE0_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule1. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_FLASH_MEM_RULE0_RULE1_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_MEM_RULE0_RULE1_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule2. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_FLASH_MEM_RULE0_RULE2_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_MEM_RULE0_RULE2_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule3. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_FLASH_MEM_RULE0_RULE3_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_MEM_RULE0_RULE3_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule4. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_FLASH_MEM_RULE0_RULE4_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_MEM_RULE0_RULE4_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule5. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_FLASH_MEM_RULE0_RULE5_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_MEM_RULE0_RULE5_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule6. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_FLASH_MEM_RULE0_RULE6_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_MEM_RULE0_RULE6_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule7. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_FLASH_MEM_RULE0_RULE7_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_FLASH_MEM_RULE0_RULE7_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for FLASH sector 0 to sector 20. Each Flash sector
   --  is 32 Kbytes. There are 20 FLASH sectors in total.
   type SEC_CTRL_FLASH_MEM_RULE_Register is record
      --  secure control rule0. it can be set when check_reg's write_lock is
      --  '0'
      RULE0          : SEC_CTRL_FLASH_MEM_RULE0_RULE0_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  secure control rule1. it can be set when check_reg's write_lock is
      --  '0'
      RULE1          : SEC_CTRL_FLASH_MEM_RULE0_RULE1_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  secure control rule2. it can be set when check_reg's write_lock is
      --  '0'
      RULE2          : SEC_CTRL_FLASH_MEM_RULE0_RULE2_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  secure control rule3. it can be set when check_reg's write_lock is
      --  '0'
      RULE3          : SEC_CTRL_FLASH_MEM_RULE0_RULE3_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  secure control rule4. it can be set when check_reg's write_lock is
      --  '0'
      RULE4          : SEC_CTRL_FLASH_MEM_RULE0_RULE4_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  secure control rule5. it can be set when check_reg's write_lock is
      --  '0'
      RULE5          : SEC_CTRL_FLASH_MEM_RULE0_RULE5_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  secure control rule6. it can be set when check_reg's write_lock is
      --  '0'
      RULE6          : SEC_CTRL_FLASH_MEM_RULE0_RULE6_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  secure control rule7. it can be set when check_reg's write_lock is
      --  '0'
      RULE7          : SEC_CTRL_FLASH_MEM_RULE0_RULE7_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_FLASH_MEM_RULE_Register use record
      RULE0          at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      RULE1          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      RULE2          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      RULE3          at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RULE4          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      RULE5          at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RULE6          at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      RULE7          at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  secure control rule0. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_ROM_MEM_RULE0_RULE0_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_ROM_MEM_RULE0_RULE0_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule1. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_ROM_MEM_RULE0_RULE1_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_ROM_MEM_RULE0_RULE1_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule2. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_ROM_MEM_RULE0_RULE2_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_ROM_MEM_RULE0_RULE2_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule3. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_ROM_MEM_RULE0_RULE3_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_ROM_MEM_RULE0_RULE3_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule4. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_ROM_MEM_RULE0_RULE4_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_ROM_MEM_RULE0_RULE4_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule5. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_ROM_MEM_RULE0_RULE5_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_ROM_MEM_RULE0_RULE5_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule6. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_ROM_MEM_RULE0_RULE6_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_ROM_MEM_RULE0_RULE6_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule7. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_ROM_MEM_RULE0_RULE7_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_ROM_MEM_RULE0_RULE7_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for ROM sector 0 to sector 31. Each ROM sector is
   --  4 Kbytes. There are 32 ROM sectors in total.
   type SEC_CTRL_ROM_MEM_RULE_Register is record
      --  secure control rule0. it can be set when check_reg's write_lock is
      --  '0'
      RULE0          : SEC_CTRL_ROM_MEM_RULE0_RULE0_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  secure control rule1. it can be set when check_reg's write_lock is
      --  '0'
      RULE1          : SEC_CTRL_ROM_MEM_RULE0_RULE1_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  secure control rule2. it can be set when check_reg's write_lock is
      --  '0'
      RULE2          : SEC_CTRL_ROM_MEM_RULE0_RULE2_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  secure control rule3. it can be set when check_reg's write_lock is
      --  '0'
      RULE3          : SEC_CTRL_ROM_MEM_RULE0_RULE3_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  secure control rule4. it can be set when check_reg's write_lock is
      --  '0'
      RULE4          : SEC_CTRL_ROM_MEM_RULE0_RULE4_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  secure control rule5. it can be set when check_reg's write_lock is
      --  '0'
      RULE5          : SEC_CTRL_ROM_MEM_RULE0_RULE5_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  secure control rule6. it can be set when check_reg's write_lock is
      --  '0'
      RULE6          : SEC_CTRL_ROM_MEM_RULE0_RULE6_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  secure control rule7. it can be set when check_reg's write_lock is
      --  '0'
      RULE7          : SEC_CTRL_ROM_MEM_RULE0_RULE7_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_ROM_MEM_RULE_Register use record
      RULE0          at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      RULE1          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      RULE2          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      RULE3          at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RULE4          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      RULE5          at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RULE6          at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      RULE7          at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Security access rules for the whole RAMX : 0x0400_0000 - 0x0400_7FFF
   type SEC_CTRL_RAMX_SLAVE_RULE_RAMX_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAMX_SLAVE_RULE_RAMX_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAMX slaves.
   type SEC_CTRL_RAMX_SLAVE_RULE_Register is record
      --  Security access rules for the whole RAMX : 0x0400_0000 - 0x0400_7FFF
      RAMX_RULE     : SEC_CTRL_RAMX_SLAVE_RULE_RAMX_RULE_Field :=
                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAMX_SLAVE_RULE_Register use record
      RAMX_RULE     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  secure control rule0. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAMX_MEM_RULE0_RULE0_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAMX_MEM_RULE0_RULE0_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule1. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAMX_MEM_RULE0_RULE1_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAMX_MEM_RULE0_RULE1_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule2. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAMX_MEM_RULE0_RULE2_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAMX_MEM_RULE0_RULE2_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule3. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAMX_MEM_RULE0_RULE3_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAMX_MEM_RULE0_RULE3_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule4. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAMX_MEM_RULE0_RULE4_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAMX_MEM_RULE0_RULE4_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule5. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAMX_MEM_RULE0_RULE5_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAMX_MEM_RULE0_RULE5_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule6. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAMX_MEM_RULE0_RULE6_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAMX_MEM_RULE0_RULE6_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule7. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAMX_MEM_RULE0_RULE7_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAMX_MEM_RULE0_RULE7_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAMX slaves.
   type SEC_CTRL_RAMX_MEM_RULE0_Register is record
      --  secure control rule0. it can be set when check_reg's write_lock is
      --  '0'
      RULE0          : SEC_CTRL_RAMX_MEM_RULE0_RULE0_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  secure control rule1. it can be set when check_reg's write_lock is
      --  '0'
      RULE1          : SEC_CTRL_RAMX_MEM_RULE0_RULE1_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  secure control rule2. it can be set when check_reg's write_lock is
      --  '0'
      RULE2          : SEC_CTRL_RAMX_MEM_RULE0_RULE2_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  secure control rule3. it can be set when check_reg's write_lock is
      --  '0'
      RULE3          : SEC_CTRL_RAMX_MEM_RULE0_RULE3_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  secure control rule4. it can be set when check_reg's write_lock is
      --  '0'
      RULE4          : SEC_CTRL_RAMX_MEM_RULE0_RULE4_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  secure control rule5. it can be set when check_reg's write_lock is
      --  '0'
      RULE5          : SEC_CTRL_RAMX_MEM_RULE0_RULE5_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  secure control rule6. it can be set when check_reg's write_lock is
      --  '0'
      RULE6          : SEC_CTRL_RAMX_MEM_RULE0_RULE6_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  secure control rule7. it can be set when check_reg's write_lock is
      --  '0'
      RULE7          : SEC_CTRL_RAMX_MEM_RULE0_RULE7_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAMX_MEM_RULE0_Register use record
      RULE0          at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      RULE1          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      RULE2          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      RULE3          at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RULE4          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      RULE5          at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RULE6          at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      RULE7          at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Security access rules for the whole RAM0 : 0x2000_0000 - 0x2000_FFFF
   type SEC_CTRL_RAM0_SLAVE_RULE_RAM0_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM0_SLAVE_RULE_RAM0_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM0 slaves.
   type SEC_CTRL_RAM0_SLAVE_RULE_Register is record
      --  Security access rules for the whole RAM0 : 0x2000_0000 - 0x2000_FFFF
      RAM0_RULE     : SEC_CTRL_RAM0_SLAVE_RULE_RAM0_RULE_Field :=
                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM0_SLAVE_RULE_Register use record
      RAM0_RULE     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  secure control rule0. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM0_MEM_RULE0_RULE0_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM0_MEM_RULE0_RULE0_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule1. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM0_MEM_RULE0_RULE1_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM0_MEM_RULE0_RULE1_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule2. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM0_MEM_RULE0_RULE2_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM0_MEM_RULE0_RULE2_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule3. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM0_MEM_RULE0_RULE3_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM0_MEM_RULE0_RULE3_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule4. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM0_MEM_RULE0_RULE4_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM0_MEM_RULE0_RULE4_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule5. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM0_MEM_RULE0_RULE5_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM0_MEM_RULE0_RULE5_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule6. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM0_MEM_RULE0_RULE6_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM0_MEM_RULE0_RULE6_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule7. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM0_MEM_RULE0_RULE7_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM0_MEM_RULE0_RULE7_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM0 slaves.
   type SEC_CTRL_RAM0_MEM_RULE_Register is record
      --  secure control rule0. it can be set when check_reg's write_lock is
      --  '0'
      RULE0          : SEC_CTRL_RAM0_MEM_RULE0_RULE0_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  secure control rule1. it can be set when check_reg's write_lock is
      --  '0'
      RULE1          : SEC_CTRL_RAM0_MEM_RULE0_RULE1_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  secure control rule2. it can be set when check_reg's write_lock is
      --  '0'
      RULE2          : SEC_CTRL_RAM0_MEM_RULE0_RULE2_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  secure control rule3. it can be set when check_reg's write_lock is
      --  '0'
      RULE3          : SEC_CTRL_RAM0_MEM_RULE0_RULE3_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  secure control rule4. it can be set when check_reg's write_lock is
      --  '0'
      RULE4          : SEC_CTRL_RAM0_MEM_RULE0_RULE4_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  secure control rule5. it can be set when check_reg's write_lock is
      --  '0'
      RULE5          : SEC_CTRL_RAM0_MEM_RULE0_RULE5_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  secure control rule6. it can be set when check_reg's write_lock is
      --  '0'
      RULE6          : SEC_CTRL_RAM0_MEM_RULE0_RULE6_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  secure control rule7. it can be set when check_reg's write_lock is
      --  '0'
      RULE7          : SEC_CTRL_RAM0_MEM_RULE0_RULE7_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM0_MEM_RULE_Register use record
      RULE0          at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      RULE1          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      RULE2          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      RULE3          at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RULE4          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      RULE5          at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RULE6          at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      RULE7          at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Security access rules for the whole RAM1 : 0x2001_0000 - 0x2001_FFFF"
   --  name="0
   type SEC_CTRL_RAM1_SLAVE_RULE_RAM1_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM1_SLAVE_RULE_RAM1_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM1 slaves.
   type SEC_CTRL_RAM1_SLAVE_RULE_Register is record
      --  Security access rules for the whole RAM1 : 0x2001_0000 - 0x2001_FFFF"
      --  name="0
      RAM1_RULE     : SEC_CTRL_RAM1_SLAVE_RULE_RAM1_RULE_Field :=
                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM1_SLAVE_RULE_Register use record
      RAM1_RULE     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  secure control rule0. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM1_MEM_RULE0_RULE0_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM1_MEM_RULE0_RULE0_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule1. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM1_MEM_RULE0_RULE1_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM1_MEM_RULE0_RULE1_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule2. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM1_MEM_RULE0_RULE2_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM1_MEM_RULE0_RULE2_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule3. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM1_MEM_RULE0_RULE3_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM1_MEM_RULE0_RULE3_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule4. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM1_MEM_RULE0_RULE4_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM1_MEM_RULE0_RULE4_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule5. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM1_MEM_RULE0_RULE5_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM1_MEM_RULE0_RULE5_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule6. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM1_MEM_RULE0_RULE6_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM1_MEM_RULE0_RULE6_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule7. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM1_MEM_RULE0_RULE7_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM1_MEM_RULE0_RULE7_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM1 slaves.
   type SEC_CTRL_RAM1_MEM_RULE_Register is record
      --  secure control rule0. it can be set when check_reg's write_lock is
      --  '0'
      RULE0          : SEC_CTRL_RAM1_MEM_RULE0_RULE0_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  secure control rule1. it can be set when check_reg's write_lock is
      --  '0'
      RULE1          : SEC_CTRL_RAM1_MEM_RULE0_RULE1_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  secure control rule2. it can be set when check_reg's write_lock is
      --  '0'
      RULE2          : SEC_CTRL_RAM1_MEM_RULE0_RULE2_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  secure control rule3. it can be set when check_reg's write_lock is
      --  '0'
      RULE3          : SEC_CTRL_RAM1_MEM_RULE0_RULE3_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  secure control rule4. it can be set when check_reg's write_lock is
      --  '0'
      RULE4          : SEC_CTRL_RAM1_MEM_RULE0_RULE4_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  secure control rule5. it can be set when check_reg's write_lock is
      --  '0'
      RULE5          : SEC_CTRL_RAM1_MEM_RULE0_RULE5_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  secure control rule6. it can be set when check_reg's write_lock is
      --  '0'
      RULE6          : SEC_CTRL_RAM1_MEM_RULE0_RULE6_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  secure control rule7. it can be set when check_reg's write_lock is
      --  '0'
      RULE7          : SEC_CTRL_RAM1_MEM_RULE0_RULE7_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM1_MEM_RULE_Register use record
      RULE0          at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      RULE1          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      RULE2          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      RULE3          at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RULE4          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      RULE5          at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RULE6          at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      RULE7          at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Security access rules for the whole RAM2 : 0x2002_0000 - 0x2002_FFFF
   type SEC_CTRL_RAM2_SLAVE_RULE_RAM2_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM2_SLAVE_RULE_RAM2_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM2 slaves.
   type SEC_CTRL_RAM2_SLAVE_RULE_Register is record
      --  Security access rules for the whole RAM2 : 0x2002_0000 - 0x2002_FFFF
      RAM2_RULE     : SEC_CTRL_RAM2_SLAVE_RULE_RAM2_RULE_Field :=
                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM2_SLAVE_RULE_Register use record
      RAM2_RULE     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  secure control rule0. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM2_MEM_RULE0_RULE0_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM2_MEM_RULE0_RULE0_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule1. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM2_MEM_RULE0_RULE1_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM2_MEM_RULE0_RULE1_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule2. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM2_MEM_RULE0_RULE2_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM2_MEM_RULE0_RULE2_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule3. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM2_MEM_RULE0_RULE3_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM2_MEM_RULE0_RULE3_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule4. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM2_MEM_RULE0_RULE4_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM2_MEM_RULE0_RULE4_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule5. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM2_MEM_RULE0_RULE5_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM2_MEM_RULE0_RULE5_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule6. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM2_MEM_RULE0_RULE6_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM2_MEM_RULE0_RULE6_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule7. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM2_MEM_RULE0_RULE7_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM2_MEM_RULE0_RULE7_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM2 slaves.
   type SEC_CTRL_RAM2_MEM_RULE_Register is record
      --  secure control rule0. it can be set when check_reg's write_lock is
      --  '0'
      RULE0          : SEC_CTRL_RAM2_MEM_RULE0_RULE0_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  secure control rule1. it can be set when check_reg's write_lock is
      --  '0'
      RULE1          : SEC_CTRL_RAM2_MEM_RULE0_RULE1_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  secure control rule2. it can be set when check_reg's write_lock is
      --  '0'
      RULE2          : SEC_CTRL_RAM2_MEM_RULE0_RULE2_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  secure control rule3. it can be set when check_reg's write_lock is
      --  '0'
      RULE3          : SEC_CTRL_RAM2_MEM_RULE0_RULE3_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  secure control rule4. it can be set when check_reg's write_lock is
      --  '0'
      RULE4          : SEC_CTRL_RAM2_MEM_RULE0_RULE4_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  secure control rule5. it can be set when check_reg's write_lock is
      --  '0'
      RULE5          : SEC_CTRL_RAM2_MEM_RULE0_RULE5_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  secure control rule6. it can be set when check_reg's write_lock is
      --  '0'
      RULE6          : SEC_CTRL_RAM2_MEM_RULE0_RULE6_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  secure control rule7. it can be set when check_reg's write_lock is
      --  '0'
      RULE7          : SEC_CTRL_RAM2_MEM_RULE0_RULE7_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM2_MEM_RULE_Register use record
      RULE0          at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      RULE1          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      RULE2          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      RULE3          at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RULE4          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      RULE5          at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RULE6          at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      RULE7          at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Security access rules for the whole RAM3: 0x2003_0000 - 0x2003_FFFF
   type SEC_CTRL_RAM3_SLAVE_RULE_RAM3_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM3_SLAVE_RULE_RAM3_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM3 slaves.
   type SEC_CTRL_RAM3_SLAVE_RULE_Register is record
      --  Security access rules for the whole RAM3: 0x2003_0000 - 0x2003_FFFF
      RAM3_RULE     : SEC_CTRL_RAM3_SLAVE_RULE_RAM3_RULE_Field :=
                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM3_SLAVE_RULE_Register use record
      RAM3_RULE     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  secure control rule0. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM3_MEM_RULE0_RULE0_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM3_MEM_RULE0_RULE0_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule1. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM3_MEM_RULE0_RULE1_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM3_MEM_RULE0_RULE1_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule2. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM3_MEM_RULE0_RULE2_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM3_MEM_RULE0_RULE2_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule3. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM3_MEM_RULE0_RULE3_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM3_MEM_RULE0_RULE3_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule4. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM3_MEM_RULE0_RULE4_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM3_MEM_RULE0_RULE4_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule5. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM3_MEM_RULE0_RULE5_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM3_MEM_RULE0_RULE5_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule6. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM3_MEM_RULE0_RULE6_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM3_MEM_RULE0_RULE6_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule7. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM3_MEM_RULE0_RULE7_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM3_MEM_RULE0_RULE7_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM3 slaves.
   type SEC_CTRL_RAM3_MEM_RULE_Register is record
      --  secure control rule0. it can be set when check_reg's write_lock is
      --  '0'
      RULE0          : SEC_CTRL_RAM3_MEM_RULE0_RULE0_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  secure control rule1. it can be set when check_reg's write_lock is
      --  '0'
      RULE1          : SEC_CTRL_RAM3_MEM_RULE0_RULE1_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  secure control rule2. it can be set when check_reg's write_lock is
      --  '0'
      RULE2          : SEC_CTRL_RAM3_MEM_RULE0_RULE2_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  secure control rule3. it can be set when check_reg's write_lock is
      --  '0'
      RULE3          : SEC_CTRL_RAM3_MEM_RULE0_RULE3_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  secure control rule4. it can be set when check_reg's write_lock is
      --  '0'
      RULE4          : SEC_CTRL_RAM3_MEM_RULE0_RULE4_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  secure control rule5. it can be set when check_reg's write_lock is
      --  '0'
      RULE5          : SEC_CTRL_RAM3_MEM_RULE0_RULE5_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  secure control rule6. it can be set when check_reg's write_lock is
      --  '0'
      RULE6          : SEC_CTRL_RAM3_MEM_RULE0_RULE6_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  secure control rule7. it can be set when check_reg's write_lock is
      --  '0'
      RULE7          : SEC_CTRL_RAM3_MEM_RULE0_RULE7_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM3_MEM_RULE_Register use record
      RULE0          at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      RULE1          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      RULE2          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      RULE3          at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RULE4          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      RULE5          at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RULE6          at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      RULE7          at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Security access rules for the whole RAM4 : 0x2004_0000 - 0x2004_3FFF
   type SEC_CTRL_RAM4_SLAVE_RULE_RAM4_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM4_SLAVE_RULE_RAM4_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM4 slaves.
   type SEC_CTRL_RAM4_SLAVE_RULE_Register is record
      --  Security access rules for the whole RAM4 : 0x2004_0000 - 0x2004_3FFF
      RAM4_RULE     : SEC_CTRL_RAM4_SLAVE_RULE_RAM4_RULE_Field :=
                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM4_SLAVE_RULE_Register use record
      RAM4_RULE     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  secure control rule0. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM4_MEM_RULE0_RULE0_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM4_MEM_RULE0_RULE0_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule1. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM4_MEM_RULE0_RULE1_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM4_MEM_RULE0_RULE1_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule2. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM4_MEM_RULE0_RULE2_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM4_MEM_RULE0_RULE2_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  secure control rule3. it can be set when check_reg's write_lock is '0'
   type SEC_CTRL_RAM4_MEM_RULE0_RULE3_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_RAM4_MEM_RULE0_RULE3_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM4 slaves.
   type SEC_CTRL_RAM4_MEM_RULE0_Register is record
      --  secure control rule0. it can be set when check_reg's write_lock is
      --  '0'
      RULE0          : SEC_CTRL_RAM4_MEM_RULE0_RULE0_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  secure control rule1. it can be set when check_reg's write_lock is
      --  '0'
      RULE1          : SEC_CTRL_RAM4_MEM_RULE0_RULE1_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  secure control rule2. it can be set when check_reg's write_lock is
      --  '0'
      RULE2          : SEC_CTRL_RAM4_MEM_RULE0_RULE2_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  secure control rule3. it can be set when check_reg's write_lock is
      --  '0'
      RULE3          : SEC_CTRL_RAM4_MEM_RULE0_RULE3_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_RAM4_MEM_RULE0_Register use record
      RULE0          at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      RULE1          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      RULE2          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      RULE3          at 0 range 12 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  Security access rules for the whole APB Bridge 0
   type SEC_CTRL_APB_BRIDGE_SLAVE_RULE_APBBRIDGE0_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE_SLAVE_RULE_APBBRIDGE0_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for the whole APB Bridge 1
   type SEC_CTRL_APB_BRIDGE_SLAVE_RULE_APBBRIDGE1_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE_SLAVE_RULE_APBBRIDGE1_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for both APB Bridges slaves.
   type SEC_CTRL_APB_BRIDGE_SLAVE_RULE_Register is record
      --  Security access rules for the whole APB Bridge 0
      APBBRIDGE0_RULE : SEC_CTRL_APB_BRIDGE_SLAVE_RULE_APBBRIDGE0_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3    : HAL.UInt2 := 16#0#;
      --  Security access rules for the whole APB Bridge 1
      APBBRIDGE1_RULE : SEC_CTRL_APB_BRIDGE_SLAVE_RULE_APBBRIDGE1_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_31   : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_APB_BRIDGE_SLAVE_RULE_Register use record
      APBBRIDGE0_RULE at 0 range 0 .. 1;
      Reserved_2_3    at 0 range 2 .. 3;
      APBBRIDGE1_RULE at 0 range 4 .. 5;
      Reserved_6_31   at 0 range 6 .. 31;
   end record;

   --  System Configuration
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_SYSCON_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_SYSCON_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  I/O Configuration
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_IOCON_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_IOCON_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  GPIO input Interrupt 0
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_GINT0_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_GINT0_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  GPIO input Interrupt 1
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_GINT1_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_GINT1_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Pin Interrupt and Pattern match
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_PINT_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_PINT_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Secure Pin Interrupt and Pattern match
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_SEC_PINT_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_SEC_PINT_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Peripheral input multiplexing
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_INPUTMUX_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_INPUTMUX_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for APB Bridge 0 peripherals. Each APB bridge
   --  sector is 4 Kbytes. There are 32 APB Bridge 0 sectors in total.
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_Register is record
      --  System Configuration
      SYSCON_RULE    : SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_SYSCON_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  I/O Configuration
      IOCON_RULE     : SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_IOCON_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  GPIO input Interrupt 0
      GINT0_RULE     : SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_GINT0_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  GPIO input Interrupt 1
      GINT1_RULE     : SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_GINT1_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Pin Interrupt and Pattern match
      PINT_RULE      : SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_PINT_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Secure Pin Interrupt and Pattern match
      SEC_PINT_RULE  : SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_SEC_PINT_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Peripheral input multiplexing
      INPUTMUX_RULE  : SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_INPUTMUX_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_Register use record
      SYSCON_RULE    at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      IOCON_RULE     at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      GINT0_RULE     at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      GINT1_RULE     at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      PINT_RULE      at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      SEC_PINT_RULE  at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      INPUTMUX_RULE  at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  Standard counter/Timer 0
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_CTIMER0_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_CTIMER0_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Standard counter/Timer 1
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_CTIMER1_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_CTIMER1_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Windiwed wtachdog Timer
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_WWDT_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_WWDT_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Multi-rate Timer
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_MRT_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_MRT_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Micro-Timer
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_UTICK_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_UTICK_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for APB Bridge 0 peripherals. Each APB bridge
   --  sector is 4 Kbytes. There are 32 APB Bridge 0 sectors in total.
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_Register is record
      --  Standard counter/Timer 0
      CTIMER0_RULE   : SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_CTIMER0_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Standard counter/Timer 1
      CTIMER1_RULE   : SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_CTIMER1_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_15  : HAL.UInt10 := 16#0#;
      --  Windiwed wtachdog Timer
      WWDT_RULE      : SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_WWDT_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Multi-rate Timer
      MRT_RULE       : SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_MRT_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Micro-Timer
      UTICK_RULE     : SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_UTICK_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_Register use record
      CTIMER0_RULE   at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CTIMER1_RULE   at 0 range 4 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      WWDT_RULE      at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      MRT_RULE       at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      UTICK_RULE     at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  Analog Modules controller
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL2_ANACTRL_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL2_ANACTRL_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for APB Bridge 0 peripherals. Each APB bridge
   --  sector is 4 Kbytes. There are 32 APB Bridge 0 sectors in total.
   type SEC_CTRL_APB_BRIDGE0_MEM_CTRL2_Register is record
      --  unspecified
      Reserved_0_11  : HAL.UInt12 := 16#0#;
      --  Analog Modules controller
      ANACTRL_RULE   : SEC_CTRL_APB_BRIDGE0_MEM_CTRL2_ANACTRL_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_APB_BRIDGE0_MEM_CTRL2_Register use record
      Reserved_0_11  at 0 range 0 .. 11;
      ANACTRL_RULE   at 0 range 12 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  Power Management Controller
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL0_PMC_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL0_PMC_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  System Controller
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL0_SYSCTRL_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL0_SYSCTRL_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for APB Bridge 1 peripherals. Each APB bridge
   --  sector is 4 Kbytes. There are 32 APB Bridge 1 sectors in total.
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL0_Register is record
      --  Power Management Controller
      PMC_RULE       : SEC_CTRL_APB_BRIDGE1_MEM_CTRL0_PMC_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_11  : HAL.UInt10 := 16#0#;
      --  System Controller
      SYSCTRL_RULE   : SEC_CTRL_APB_BRIDGE1_MEM_CTRL0_SYSCTRL_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL0_Register use record
      PMC_RULE       at 0 range 0 .. 1;
      Reserved_2_11  at 0 range 2 .. 11;
      SYSCTRL_RULE   at 0 range 12 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  Standard counter/Timer 2
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_CTIMER2_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_CTIMER2_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Standard counter/Timer 3
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_CTIMER3_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_CTIMER3_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Standard counter/Timer 4
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_CTIMER4_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_CTIMER4_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Real Time Counter
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_RTC_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_RTC_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  OS Event Timer
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_OSEVENT_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_OSEVENT_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for APB Bridge 1 peripherals. Each APB bridge
   --  sector is 4 Kbytes. There are 32 APB Bridge 1 sectors in total.
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_Register is record
      --  Standard counter/Timer 2
      CTIMER2_RULE   : SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_CTIMER2_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Standard counter/Timer 3
      CTIMER3_RULE   : SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_CTIMER3_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Standard counter/Timer 4
      CTIMER4_RULE   : SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_CTIMER4_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Real Time Counter
      RTC_RULE       : SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_RTC_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  OS Event Timer
      OSEVENT_RULE   : SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_OSEVENT_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_Register use record
      CTIMER2_RULE   at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CTIMER3_RULE   at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      CTIMER4_RULE   at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      RTC_RULE       at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      OSEVENT_RULE   at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Flash Controller
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL2_FLASH_CTRL_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL2_FLASH_CTRL_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Prince
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL2_PRINCE_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL2_PRINCE_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for APB Bridge 1 peripherals. Each APB bridge
   --  sector is 4 Kbytes. There are 32 APB Bridge 1 sectors in total.
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL2_Register is record
      --  unspecified
      Reserved_0_15   : HAL.UInt16 := 16#0#;
      --  Flash Controller
      FLASH_CTRL_RULE : SEC_CTRL_APB_BRIDGE1_MEM_CTRL2_FLASH_CTRL_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19  : HAL.UInt2 := 16#0#;
      --  Prince
      PRINCE_RULE     : SEC_CTRL_APB_BRIDGE1_MEM_CTRL2_PRINCE_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_31  : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL2_Register use record
      Reserved_0_15   at 0 range 0 .. 15;
      FLASH_CTRL_RULE at 0 range 16 .. 17;
      Reserved_18_19  at 0 range 18 .. 19;
      PRINCE_RULE     at 0 range 20 .. 21;
      Reserved_22_31  at 0 range 22 .. 31;
   end record;

   --  USB High Speed Phy controller
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_USBHPHY_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_USBHPHY_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  True Random Number Generator
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_RNG_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_RNG_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  PUF
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_PUF_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_PUF_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Programmable Look-Up logic
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_PLU_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_PLU_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for APB Bridge 1 peripherals. Each APB bridge
   --  sector is 4 Kbytes. There are 32 APB Bridge 1 sectors in total.
   type SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_Register is record
      --  USB High Speed Phy controller
      USBHPHY_RULE   : SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_USBHPHY_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  True Random Number Generator
      RNG_RULE       : SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_RNG_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  PUF
      PUF_RULE       : SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_PUF_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_19 : HAL.UInt6 := 16#0#;
      --  Programmable Look-Up logic
      PLU_RULE       : SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_PLU_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_Register use record
      USBHPHY_RULE   at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      RNG_RULE       at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      PUF_RULE       at 0 range 12 .. 13;
      Reserved_14_19 at 0 range 14 .. 19;
      PLU_RULE       at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  DMA Controller
   type SEC_CTRL_AHB_PORT8_SLAVE0_RULE_DMA0_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE0_RULE_DMA0_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  USB Full-speed device
   type SEC_CTRL_AHB_PORT8_SLAVE0_RULE_FS_USB_DEV_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE0_RULE_FS_USB_DEV_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  SCTimer
   type SEC_CTRL_AHB_PORT8_SLAVE0_RULE_SCT_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE0_RULE_SCT_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Flexcomm interface 0
   type SEC_CTRL_AHB_PORT8_SLAVE0_RULE_FLEXCOMM0_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE0_RULE_FLEXCOMM0_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Flexcomm interface 1
   type SEC_CTRL_AHB_PORT8_SLAVE0_RULE_FLEXCOMM1_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE0_RULE_FLEXCOMM1_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for AHB peripherals.
   type SEC_CTRL_AHB_PORT8_SLAVE0_RULE_Register is record
      --  unspecified
      Reserved_0_7    : HAL.UInt8 := 16#0#;
      --  DMA Controller
      DMA0_RULE       : SEC_CTRL_AHB_PORT8_SLAVE0_RULE_DMA0_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_15  : HAL.UInt6 := 16#0#;
      --  USB Full-speed device
      FS_USB_DEV_RULE : SEC_CTRL_AHB_PORT8_SLAVE0_RULE_FS_USB_DEV_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19  : HAL.UInt2 := 16#0#;
      --  SCTimer
      SCT_RULE        : SEC_CTRL_AHB_PORT8_SLAVE0_RULE_SCT_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23  : HAL.UInt2 := 16#0#;
      --  Flexcomm interface 0
      FLEXCOMM0_RULE  : SEC_CTRL_AHB_PORT8_SLAVE0_RULE_FLEXCOMM0_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27  : HAL.UInt2 := 16#0#;
      --  Flexcomm interface 1
      FLEXCOMM1_RULE  : SEC_CTRL_AHB_PORT8_SLAVE0_RULE_FLEXCOMM1_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31  : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_AHB_PORT8_SLAVE0_RULE_Register use record
      Reserved_0_7    at 0 range 0 .. 7;
      DMA0_RULE       at 0 range 8 .. 9;
      Reserved_10_15  at 0 range 10 .. 15;
      FS_USB_DEV_RULE at 0 range 16 .. 17;
      Reserved_18_19  at 0 range 18 .. 19;
      SCT_RULE        at 0 range 20 .. 21;
      Reserved_22_23  at 0 range 22 .. 23;
      FLEXCOMM0_RULE  at 0 range 24 .. 25;
      Reserved_26_27  at 0 range 26 .. 27;
      FLEXCOMM1_RULE  at 0 range 28 .. 29;
      Reserved_30_31  at 0 range 30 .. 31;
   end record;

   --  Flexcomm interface 2
   type SEC_CTRL_AHB_PORT8_SLAVE1_RULE_FLEXCOMM2_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE1_RULE_FLEXCOMM2_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Flexcomm interface 3
   type SEC_CTRL_AHB_PORT8_SLAVE1_RULE_FLEXCOMM3_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE1_RULE_FLEXCOMM3_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Flexcomm interface 4
   type SEC_CTRL_AHB_PORT8_SLAVE1_RULE_FLEXCOMM4_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE1_RULE_FLEXCOMM4_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Inter CPU communication Mailbox
   type SEC_CTRL_AHB_PORT8_SLAVE1_RULE_MAILBOX_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE1_RULE_MAILBOX_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  High Speed GPIO
   type SEC_CTRL_AHB_PORT8_SLAVE1_RULE_GPIO0_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT8_SLAVE1_RULE_GPIO0_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for AHB peripherals.
   type SEC_CTRL_AHB_PORT8_SLAVE1_RULE_Register is record
      --  Flexcomm interface 2
      FLEXCOMM2_RULE : SEC_CTRL_AHB_PORT8_SLAVE1_RULE_FLEXCOMM2_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Flexcomm interface 3
      FLEXCOMM3_RULE : SEC_CTRL_AHB_PORT8_SLAVE1_RULE_FLEXCOMM3_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Flexcomm interface 4
      FLEXCOMM4_RULE : SEC_CTRL_AHB_PORT8_SLAVE1_RULE_FLEXCOMM4_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Inter CPU communication Mailbox
      MAILBOX_RULE   : SEC_CTRL_AHB_PORT8_SLAVE1_RULE_MAILBOX_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  High Speed GPIO
      GPIO0_RULE     : SEC_CTRL_AHB_PORT8_SLAVE1_RULE_GPIO0_RULE_Field :=
                        NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_AHB_PORT8_SLAVE1_RULE_Register use record
      FLEXCOMM2_RULE at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      FLEXCOMM3_RULE at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      FLEXCOMM4_RULE at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      MAILBOX_RULE   at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      GPIO0_RULE     at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  USB high Speed device registers
   type SEC_CTRL_AHB_PORT9_SLAVE0_RULE_USB_HS_DEV_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT9_SLAVE0_RULE_USB_HS_DEV_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  CRC engine
   type SEC_CTRL_AHB_PORT9_SLAVE0_RULE_CRC_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT9_SLAVE0_RULE_CRC_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Flexcomm interface 5
   type SEC_CTRL_AHB_PORT9_SLAVE0_RULE_FLEXCOMM5_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT9_SLAVE0_RULE_FLEXCOMM5_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Flexcomm interface 6
   type SEC_CTRL_AHB_PORT9_SLAVE0_RULE_FLEXCOMM6_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT9_SLAVE0_RULE_FLEXCOMM6_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for AHB peripherals.
   type SEC_CTRL_AHB_PORT9_SLAVE0_RULE_Register is record
      --  unspecified
      Reserved_0_15   : HAL.UInt16 := 16#0#;
      --  USB high Speed device registers
      USB_HS_DEV_RULE : SEC_CTRL_AHB_PORT9_SLAVE0_RULE_USB_HS_DEV_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19  : HAL.UInt2 := 16#0#;
      --  CRC engine
      CRC_RULE        : SEC_CTRL_AHB_PORT9_SLAVE0_RULE_CRC_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23  : HAL.UInt2 := 16#0#;
      --  Flexcomm interface 5
      FLEXCOMM5_RULE  : SEC_CTRL_AHB_PORT9_SLAVE0_RULE_FLEXCOMM5_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27  : HAL.UInt2 := 16#0#;
      --  Flexcomm interface 6
      FLEXCOMM6_RULE  : SEC_CTRL_AHB_PORT9_SLAVE0_RULE_FLEXCOMM6_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31  : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_AHB_PORT9_SLAVE0_RULE_Register use record
      Reserved_0_15   at 0 range 0 .. 15;
      USB_HS_DEV_RULE at 0 range 16 .. 17;
      Reserved_18_19  at 0 range 18 .. 19;
      CRC_RULE        at 0 range 20 .. 21;
      Reserved_22_23  at 0 range 22 .. 23;
      FLEXCOMM5_RULE  at 0 range 24 .. 25;
      Reserved_26_27  at 0 range 26 .. 27;
      FLEXCOMM6_RULE  at 0 range 28 .. 29;
      Reserved_30_31  at 0 range 30 .. 31;
   end record;

   --  Flexcomm interface 7
   type SEC_CTRL_AHB_PORT9_SLAVE1_RULE_FLEXCOMM7_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT9_SLAVE1_RULE_FLEXCOMM7_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  SDMMC card interface
   type SEC_CTRL_AHB_PORT9_SLAVE1_RULE_SDIO_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT9_SLAVE1_RULE_SDIO_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Debug mailbox (aka ISP-AP)
   type SEC_CTRL_AHB_PORT9_SLAVE1_RULE_DBG_MAILBOX_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT9_SLAVE1_RULE_DBG_MAILBOX_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  High Speed SPI
   type SEC_CTRL_AHB_PORT9_SLAVE1_RULE_HS_LSPI_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT9_SLAVE1_RULE_HS_LSPI_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for AHB peripherals.
   type SEC_CTRL_AHB_PORT9_SLAVE1_RULE_Register is record
      --  Flexcomm interface 7
      FLEXCOMM7_RULE   : SEC_CTRL_AHB_PORT9_SLAVE1_RULE_FLEXCOMM7_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_11    : HAL.UInt10 := 16#0#;
      --  SDMMC card interface
      SDIO_RULE        : SEC_CTRL_AHB_PORT9_SLAVE1_RULE_SDIO_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15   : HAL.UInt2 := 16#0#;
      --  Debug mailbox (aka ISP-AP)
      DBG_MAILBOX_RULE : SEC_CTRL_AHB_PORT9_SLAVE1_RULE_DBG_MAILBOX_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_27   : HAL.UInt10 := 16#0#;
      --  High Speed SPI
      HS_LSPI_RULE     : SEC_CTRL_AHB_PORT9_SLAVE1_RULE_HS_LSPI_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31   : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_AHB_PORT9_SLAVE1_RULE_Register use record
      FLEXCOMM7_RULE   at 0 range 0 .. 1;
      Reserved_2_11    at 0 range 2 .. 11;
      SDIO_RULE        at 0 range 12 .. 13;
      Reserved_14_15   at 0 range 14 .. 15;
      DBG_MAILBOX_RULE at 0 range 16 .. 17;
      Reserved_18_27   at 0 range 18 .. 27;
      HS_LSPI_RULE     at 0 range 28 .. 29;
      Reserved_30_31   at 0 range 30 .. 31;
   end record;

   --  ADC
   type SEC_CTRL_AHB_PORT10_SLAVE0_RULE_ADC_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT10_SLAVE0_RULE_ADC_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  USB Full Speed Host registers.
   type SEC_CTRL_AHB_PORT10_SLAVE0_RULE_USB_FS_HOST_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT10_SLAVE0_RULE_USB_FS_HOST_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  USB High speed host registers
   type SEC_CTRL_AHB_PORT10_SLAVE0_RULE_USB_HS_HOST_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT10_SLAVE0_RULE_USB_HS_HOST_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  SHA-2 crypto registers
   type SEC_CTRL_AHB_PORT10_SLAVE0_RULE_HASH_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT10_SLAVE0_RULE_HASH_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  RSA/ECC crypto accelerator
   type SEC_CTRL_AHB_PORT10_SLAVE0_RULE_CASPER_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT10_SLAVE0_RULE_CASPER_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Power Quad (CPU0 processor hardware accelerator)
   type SEC_CTRL_AHB_PORT10_SLAVE0_RULE_PQ_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT10_SLAVE0_RULE_PQ_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  DMA Controller (Secure)
   type SEC_CTRL_AHB_PORT10_SLAVE0_RULE_DMA1_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT10_SLAVE0_RULE_DMA1_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for AHB peripherals.
   type SEC_CTRL_AHB_PORT10_SLAVE0_RULE_Register is record
      --  ADC
      ADC_RULE         : SEC_CTRL_AHB_PORT10_SLAVE0_RULE_ADC_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_7     : HAL.UInt6 := 16#0#;
      --  USB Full Speed Host registers.
      USB_FS_HOST_RULE : SEC_CTRL_AHB_PORT10_SLAVE0_RULE_USB_FS_HOST_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11   : HAL.UInt2 := 16#0#;
      --  USB High speed host registers
      USB_HS_HOST_RULE : SEC_CTRL_AHB_PORT10_SLAVE0_RULE_USB_HS_HOST_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_15   : HAL.UInt2 := 16#0#;
      --  SHA-2 crypto registers
      HASH_RULE        : SEC_CTRL_AHB_PORT10_SLAVE0_RULE_HASH_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_18_19   : HAL.UInt2 := 16#0#;
      --  RSA/ECC crypto accelerator
      CASPER_RULE      : SEC_CTRL_AHB_PORT10_SLAVE0_RULE_CASPER_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_22_23   : HAL.UInt2 := 16#0#;
      --  Power Quad (CPU0 processor hardware accelerator)
      PQ_RULE          : SEC_CTRL_AHB_PORT10_SLAVE0_RULE_PQ_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_27   : HAL.UInt2 := 16#0#;
      --  DMA Controller (Secure)
      DMA1_RULE        : SEC_CTRL_AHB_PORT10_SLAVE0_RULE_DMA1_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_30_31   : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_AHB_PORT10_SLAVE0_RULE_Register use record
      ADC_RULE         at 0 range 0 .. 1;
      Reserved_2_7     at 0 range 2 .. 7;
      USB_FS_HOST_RULE at 0 range 8 .. 9;
      Reserved_10_11   at 0 range 10 .. 11;
      USB_HS_HOST_RULE at 0 range 12 .. 13;
      Reserved_14_15   at 0 range 14 .. 15;
      HASH_RULE        at 0 range 16 .. 17;
      Reserved_18_19   at 0 range 18 .. 19;
      CASPER_RULE      at 0 range 20 .. 21;
      Reserved_22_23   at 0 range 22 .. 23;
      PQ_RULE          at 0 range 24 .. 25;
      Reserved_26_27   at 0 range 26 .. 27;
      DMA1_RULE        at 0 range 28 .. 29;
      Reserved_30_31   at 0 range 30 .. 31;
   end record;

   --  Secure High Speed GPIO
   type SEC_CTRL_AHB_PORT10_SLAVE1_RULE_GPIO1_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT10_SLAVE1_RULE_GPIO1_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  AHB Secure Controller
   type SEC_CTRL_AHB_PORT10_SLAVE1_RULE_AHB_SEC_CTRL_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_PORT10_SLAVE1_RULE_AHB_SEC_CTRL_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for AHB peripherals.
   type SEC_CTRL_AHB_PORT10_SLAVE1_RULE_Register is record
      --  Secure High Speed GPIO
      GPIO1_RULE        : SEC_CTRL_AHB_PORT10_SLAVE1_RULE_GPIO1_RULE_Field :=
                           NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3      : HAL.UInt2 := 16#0#;
      --  AHB Secure Controller
      AHB_SEC_CTRL_RULE : SEC_CTRL_AHB_PORT10_SLAVE1_RULE_AHB_SEC_CTRL_RULE_Field :=
                           NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_31     : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_AHB_PORT10_SLAVE1_RULE_Register use record
      GPIO1_RULE        at 0 range 0 .. 1;
      Reserved_2_3      at 0 range 2 .. 3;
      AHB_SEC_CTRL_RULE at 0 range 4 .. 5;
      Reserved_6_31     at 0 range 6 .. 31;
   end record;

   --  Address space: 0x400A_0000 - 0x400A_CFFF
   type SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_0_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_0_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Address space: 0x400A_D000 - 0x400A_DFFF
   type SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_1_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_1_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Address space: 0x400A_E000 - 0x400A_EFFF
   type SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_2_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_2_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Address space: 0x400A_F000 - 0x400A_FFFF
   type SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_3_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_3_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for AHB_SEC_CTRL_AHB.
   type SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_Register is record
      --  Address space: 0x400A_0000 - 0x400A_CFFF
      AHB_SEC_CTRL_SECT_0_RULE : SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_0_RULE_Field :=
                                  NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3             : HAL.UInt2 := 16#0#;
      --  Address space: 0x400A_D000 - 0x400A_DFFF
      AHB_SEC_CTRL_SECT_1_RULE : SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_1_RULE_Field :=
                                  NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7             : HAL.UInt2 := 16#0#;
      --  Address space: 0x400A_E000 - 0x400A_EFFF
      AHB_SEC_CTRL_SECT_2_RULE : SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_2_RULE_Field :=
                                  NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11           : HAL.UInt2 := 16#0#;
      --  Address space: 0x400A_F000 - 0x400A_FFFF
      AHB_SEC_CTRL_SECT_3_RULE : SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_AHB_SEC_CTRL_SECT_3_RULE_Field :=
                                  NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_31           : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_Register use record
      AHB_SEC_CTRL_SECT_0_RULE at 0 range 0 .. 1;
      Reserved_2_3             at 0 range 2 .. 3;
      AHB_SEC_CTRL_SECT_1_RULE at 0 range 4 .. 5;
      Reserved_6_7             at 0 range 6 .. 7;
      AHB_SEC_CTRL_SECT_2_RULE at 0 range 8 .. 9;
      Reserved_10_11           at 0 range 10 .. 11;
      AHB_SEC_CTRL_SECT_3_RULE at 0 range 12 .. 13;
      Reserved_14_31           at 0 range 14 .. 31;
   end record;

   --  Security access rules for the whole USB High Speed RAM : 0x4010_0000 -
   --  0x4010_3FFF
   type SEC_CTRL_USB_HS_SLAVE_RULE_RAM_USB_HS_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_USB_HS_SLAVE_RULE_RAM_USB_HS_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for USB High speed RAM slaves.
   type SEC_CTRL_USB_HS_SLAVE_RULE_Register is record
      --  Security access rules for the whole USB High Speed RAM : 0x4010_0000
      --  - 0x4010_3FFF
      RAM_USB_HS_RULE : SEC_CTRL_USB_HS_SLAVE_RULE_RAM_USB_HS_RULE_Field :=
                         NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_31   : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_USB_HS_SLAVE_RULE_Register use record
      RAM_USB_HS_RULE at 0 range 0 .. 1;
      Reserved_2_31   at 0 range 2 .. 31;
   end record;

   --  Address space: 0x4010_0000 - 0x4010_0FFF
   type SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_0_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_0_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Address space: 0x4010_1000 - 0x4010_1FFF
   type SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_1_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_1_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Address space: 0x4010_2000 - 0x4010_2FFF
   type SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_2_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_2_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Address space: 0x4010_3000 - 0x4010_3FFF
   type SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_3_RULE_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_3_RULE_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Security access rules for RAM_USB_HS.
   type SEC_CTRL_USB_HS_MEM_RULE_Register is record
      --  Address space: 0x4010_0000 - 0x4010_0FFF
      SRAM_SECT_0_RULE : SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_0_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_2_3     : HAL.UInt2 := 16#0#;
      --  Address space: 0x4010_1000 - 0x4010_1FFF
      SRAM_SECT_1_RULE : SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_1_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_6_7     : HAL.UInt2 := 16#0#;
      --  Address space: 0x4010_2000 - 0x4010_2FFF
      SRAM_SECT_2_RULE : SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_2_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_10_11   : HAL.UInt2 := 16#0#;
      --  Address space: 0x4010_3000 - 0x4010_3FFF
      SRAM_SECT_3_RULE : SEC_CTRL_USB_HS_MEM_RULE_SRAM_SECT_3_RULE_Field :=
                          NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_14_31   : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CTRL_USB_HS_MEM_RULE_Register use record
      SRAM_SECT_0_RULE at 0 range 0 .. 1;
      Reserved_2_3     at 0 range 2 .. 3;
      SRAM_SECT_1_RULE at 0 range 4 .. 5;
      Reserved_6_7     at 0 range 6 .. 7;
      SRAM_SECT_2_RULE at 0 range 8 .. 9;
      Reserved_10_11   at 0 range 10 .. 11;
      SRAM_SECT_3_RULE at 0 range 12 .. 13;
      Reserved_14_31   at 0 range 14 .. 31;
   end record;

   --  most recent security violation address for AHB port n

   --  most recent security violation address for AHB port n
   type sec_vio_addr_Registers is array (0 .. 11) of HAL.UInt32
     with Volatile;

   --  security violation access read/write indicator.
   type sec_vio_misc_info_SEC_VIO_INFO_WRITE_Field is
     (
      --  Read access.
      Read,
      --  Write access.
      Write)
     with Size => 1;
   for sec_vio_misc_info_SEC_VIO_INFO_WRITE_Field use
     (Read => 0,
      Write => 1);

   --  security violation access data/code indicator.
   type sec_vio_misc_info_SEC_VIO_INFO_DATA_ACCESS_Field is
     (
      --  Code access.
      Code,
      --  Data access.
      Data)
     with Size => 1;
   for sec_vio_misc_info_SEC_VIO_INFO_DATA_ACCESS_Field use
     (Code => 0,
      Data => 1);

   subtype sec_vio_misc_info_SEC_VIO_INFO_MASTER_SEC_LEVEL_Field is HAL.UInt4;

   --  security violation master number
   type sec_vio_misc_info_SEC_VIO_INFO_MASTER_Field is
     (
      --  CPU0 Code.
      Value_0,
      --  CPU0 System.
      Value_1,
      --  CPU1 Data.
      Value_2,
      --  CPU1 System.
      Value_3,
      --  USB-HS Device.
      Value_4,
      --  SDMA0.
      Value_5,
      --  SDIO.
      Value_8,
      --  PowerQuad.
      Value_9,
      --  HASH.
      Value_10,
      --  USB-FS Host.
      Value_11,
      --  SDMA1.
      Value_12)
     with Size => 4;
   for sec_vio_misc_info_SEC_VIO_INFO_MASTER_Field use
     (Value_0 => 0,
      Value_1 => 1,
      Value_2 => 2,
      Value_3 => 3,
      Value_4 => 4,
      Value_5 => 5,
      Value_8 => 8,
      Value_9 => 9,
      Value_10 => 10,
      Value_11 => 11,
      Value_12 => 12);

   --  most recent security violation miscellaneous information for AHB port n
   type sec_vio_misc_info_Register is record
      --  Read-only. security violation access read/write indicator.
      SEC_VIO_INFO_WRITE            : sec_vio_misc_info_SEC_VIO_INFO_WRITE_Field;
      --  Read-only. security violation access data/code indicator.
      SEC_VIO_INFO_DATA_ACCESS      : sec_vio_misc_info_SEC_VIO_INFO_DATA_ACCESS_Field;
      --  unspecified
      Reserved_2_3                  : HAL.UInt2;
      --  Read-only. bit [5:4]: master sec level and privilege level bit [7:6]:
      --  anti-pol value for master sec level and privilege level
      SEC_VIO_INFO_MASTER_SEC_LEVEL : sec_vio_misc_info_SEC_VIO_INFO_MASTER_SEC_LEVEL_Field;
      --  Read-only. security violation master number
      SEC_VIO_INFO_MASTER           : sec_vio_misc_info_SEC_VIO_INFO_MASTER_Field;
      --  unspecified
      Reserved_12_31                : HAL.UInt20;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for sec_vio_misc_info_Register use record
      SEC_VIO_INFO_WRITE            at 0 range 0 .. 0;
      SEC_VIO_INFO_DATA_ACCESS      at 0 range 1 .. 1;
      Reserved_2_3                  at 0 range 2 .. 3;
      SEC_VIO_INFO_MASTER_SEC_LEVEL at 0 range 4 .. 7;
      SEC_VIO_INFO_MASTER           at 0 range 8 .. 11;
      Reserved_12_31                at 0 range 12 .. 31;
   end record;

   --  most recent security violation miscellaneous information for AHB port n
   type sec_vio_misc_info_Registers is array (0 .. 11)
     of sec_vio_misc_info_Register
     with Volatile;

   --  violation information valid flag for AHB port 0. Write 1 to clear.
   type SEC_VIO_INFO_VALID_VIO_INFO_VALID0_Field is
     (
      --  Not valid.
      Not_Valid,
      --  Valid (violation occurred).
      Valid)
     with Size => 1;
   for SEC_VIO_INFO_VALID_VIO_INFO_VALID0_Field use
     (Not_Valid => 0,
      Valid => 1);

   --  SEC_VIO_INFO_VALID_VIO_INFO_VALID array
   type SEC_VIO_INFO_VALID_VIO_INFO_VALID_Field_Array is array (0 .. 11)
     of SEC_VIO_INFO_VALID_VIO_INFO_VALID0_Field
     with Component_Size => 1, Size => 12;

   --  Type definition for SEC_VIO_INFO_VALID_VIO_INFO_VALID
   type SEC_VIO_INFO_VALID_VIO_INFO_VALID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  VIO_INFO_VALID as a value
            Val : HAL.UInt12;
         when True =>
            --  VIO_INFO_VALID as an array
            Arr : SEC_VIO_INFO_VALID_VIO_INFO_VALID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for SEC_VIO_INFO_VALID_VIO_INFO_VALID_Field use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  security violation address/information registers valid flags
   type SEC_VIO_INFO_VALID_Register is record
      --  violation information valid flag for AHB port 0. Write 1 to clear.
      VIO_INFO_VALID : SEC_VIO_INFO_VALID_VIO_INFO_VALID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_VIO_INFO_VALID_Register use record
      VIO_INFO_VALID at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Secure mask for pin P0_0
   type SEC_GPIO_MASK0_PIO0_PIN0_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN0_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_1
   type SEC_GPIO_MASK0_PIO0_PIN1_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN1_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_2
   type SEC_GPIO_MASK0_PIO0_PIN2_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN2_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_3
   type SEC_GPIO_MASK0_PIO0_PIN3_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN3_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_4
   type SEC_GPIO_MASK0_PIO0_PIN4_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN4_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_5
   type SEC_GPIO_MASK0_PIO0_PIN5_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN5_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_6
   type SEC_GPIO_MASK0_PIO0_PIN6_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN6_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_7
   type SEC_GPIO_MASK0_PIO0_PIN7_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN7_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_8
   type SEC_GPIO_MASK0_PIO0_PIN8_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN8_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_9
   type SEC_GPIO_MASK0_PIO0_PIN9_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN9_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_10
   type SEC_GPIO_MASK0_PIO0_PIN10_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN10_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_11
   type SEC_GPIO_MASK0_PIO0_PIN11_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN11_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_12
   type SEC_GPIO_MASK0_PIO0_PIN12_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN12_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_13
   type SEC_GPIO_MASK0_PIO0_PIN13_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN13_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_14
   type SEC_GPIO_MASK0_PIO0_PIN14_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN14_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_15
   type SEC_GPIO_MASK0_PIO0_PIN15_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN15_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_16
   type SEC_GPIO_MASK0_PIO0_PIN16_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN16_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_17
   type SEC_GPIO_MASK0_PIO0_PIN17_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN17_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_18
   type SEC_GPIO_MASK0_PIO0_PIN18_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN18_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_19
   type SEC_GPIO_MASK0_PIO0_PIN19_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN19_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_20
   type SEC_GPIO_MASK0_PIO0_PIN20_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN20_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_21
   type SEC_GPIO_MASK0_PIO0_PIN21_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN21_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_22
   type SEC_GPIO_MASK0_PIO0_PIN22_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN22_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_23
   type SEC_GPIO_MASK0_PIO0_PIN23_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN23_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_24
   type SEC_GPIO_MASK0_PIO0_PIN24_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN24_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_25
   type SEC_GPIO_MASK0_PIO0_PIN25_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN25_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_26
   type SEC_GPIO_MASK0_PIO0_PIN26_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN26_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_27
   type SEC_GPIO_MASK0_PIO0_PIN27_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN27_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_28
   type SEC_GPIO_MASK0_PIO0_PIN28_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN28_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_29
   type SEC_GPIO_MASK0_PIO0_PIN29_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN29_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_30
   type SEC_GPIO_MASK0_PIO0_PIN30_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN30_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P0_31
   type SEC_GPIO_MASK0_PIO0_PIN31_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK0_PIO0_PIN31_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure GPIO mask for port 0 pins.
   type SEC_GPIO_MASK0_Register is record
      --  Secure mask for pin P0_0
      PIO0_PIN0_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN0_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_1
      PIO0_PIN1_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN1_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_2
      PIO0_PIN2_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN2_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_3
      PIO0_PIN3_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN3_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_4
      PIO0_PIN4_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN4_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_5
      PIO0_PIN5_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN5_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_6
      PIO0_PIN6_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN6_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_7
      PIO0_PIN7_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN7_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_8
      PIO0_PIN8_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN8_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_9
      PIO0_PIN9_SEC_MASK  : SEC_GPIO_MASK0_PIO0_PIN9_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_10
      PIO0_PIN10_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN10_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_11
      PIO0_PIN11_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN11_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_12
      PIO0_PIN12_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN12_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_13
      PIO0_PIN13_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN13_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_14
      PIO0_PIN14_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN14_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_15
      PIO0_PIN15_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN15_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_16
      PIO0_PIN16_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN16_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_17
      PIO0_PIN17_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN17_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_18
      PIO0_PIN18_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN18_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_19
      PIO0_PIN19_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN19_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_20
      PIO0_PIN20_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN20_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_21
      PIO0_PIN21_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN21_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_22
      PIO0_PIN22_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN22_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_23
      PIO0_PIN23_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN23_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_24
      PIO0_PIN24_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN24_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_25
      PIO0_PIN25_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN25_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_26
      PIO0_PIN26_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN26_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_27
      PIO0_PIN27_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN27_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_28
      PIO0_PIN28_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN28_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_29
      PIO0_PIN29_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN29_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_30
      PIO0_PIN30_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN30_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P0_31
      PIO0_PIN31_SEC_MASK : SEC_GPIO_MASK0_PIO0_PIN31_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_GPIO_MASK0_Register use record
      PIO0_PIN0_SEC_MASK  at 0 range 0 .. 0;
      PIO0_PIN1_SEC_MASK  at 0 range 1 .. 1;
      PIO0_PIN2_SEC_MASK  at 0 range 2 .. 2;
      PIO0_PIN3_SEC_MASK  at 0 range 3 .. 3;
      PIO0_PIN4_SEC_MASK  at 0 range 4 .. 4;
      PIO0_PIN5_SEC_MASK  at 0 range 5 .. 5;
      PIO0_PIN6_SEC_MASK  at 0 range 6 .. 6;
      PIO0_PIN7_SEC_MASK  at 0 range 7 .. 7;
      PIO0_PIN8_SEC_MASK  at 0 range 8 .. 8;
      PIO0_PIN9_SEC_MASK  at 0 range 9 .. 9;
      PIO0_PIN10_SEC_MASK at 0 range 10 .. 10;
      PIO0_PIN11_SEC_MASK at 0 range 11 .. 11;
      PIO0_PIN12_SEC_MASK at 0 range 12 .. 12;
      PIO0_PIN13_SEC_MASK at 0 range 13 .. 13;
      PIO0_PIN14_SEC_MASK at 0 range 14 .. 14;
      PIO0_PIN15_SEC_MASK at 0 range 15 .. 15;
      PIO0_PIN16_SEC_MASK at 0 range 16 .. 16;
      PIO0_PIN17_SEC_MASK at 0 range 17 .. 17;
      PIO0_PIN18_SEC_MASK at 0 range 18 .. 18;
      PIO0_PIN19_SEC_MASK at 0 range 19 .. 19;
      PIO0_PIN20_SEC_MASK at 0 range 20 .. 20;
      PIO0_PIN21_SEC_MASK at 0 range 21 .. 21;
      PIO0_PIN22_SEC_MASK at 0 range 22 .. 22;
      PIO0_PIN23_SEC_MASK at 0 range 23 .. 23;
      PIO0_PIN24_SEC_MASK at 0 range 24 .. 24;
      PIO0_PIN25_SEC_MASK at 0 range 25 .. 25;
      PIO0_PIN26_SEC_MASK at 0 range 26 .. 26;
      PIO0_PIN27_SEC_MASK at 0 range 27 .. 27;
      PIO0_PIN28_SEC_MASK at 0 range 28 .. 28;
      PIO0_PIN29_SEC_MASK at 0 range 29 .. 29;
      PIO0_PIN30_SEC_MASK at 0 range 30 .. 30;
      PIO0_PIN31_SEC_MASK at 0 range 31 .. 31;
   end record;

   --  Secure mask for pin P1_0
   type SEC_GPIO_MASK1_PIO1_PIN0_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN0_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_1
   type SEC_GPIO_MASK1_PIO1_PIN1_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN1_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_2
   type SEC_GPIO_MASK1_PIO1_PIN2_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN2_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_3
   type SEC_GPIO_MASK1_PIO1_PIN3_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN3_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_4
   type SEC_GPIO_MASK1_PIO1_PIN4_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN4_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_5
   type SEC_GPIO_MASK1_PIO1_PIN5_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN5_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_6
   type SEC_GPIO_MASK1_PIO1_PIN6_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN6_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_7
   type SEC_GPIO_MASK1_PIO1_PIN7_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN7_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_8
   type SEC_GPIO_MASK1_PIO1_PIN8_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN8_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_9
   type SEC_GPIO_MASK1_PIO1_PIN9_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN9_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_10
   type SEC_GPIO_MASK1_PIO1_PIN10_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN10_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_11
   type SEC_GPIO_MASK1_PIO1_PIN11_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN11_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_12
   type SEC_GPIO_MASK1_PIO1_PIN12_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN12_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_13
   type SEC_GPIO_MASK1_PIO1_PIN13_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN13_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_14
   type SEC_GPIO_MASK1_PIO1_PIN14_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN14_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_15
   type SEC_GPIO_MASK1_PIO1_PIN15_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN15_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_16
   type SEC_GPIO_MASK1_PIO1_PIN16_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN16_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_17
   type SEC_GPIO_MASK1_PIO1_PIN17_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN17_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_18
   type SEC_GPIO_MASK1_PIO1_PIN18_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN18_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_19
   type SEC_GPIO_MASK1_PIO1_PIN19_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN19_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_20
   type SEC_GPIO_MASK1_PIO1_PIN20_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN20_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_21
   type SEC_GPIO_MASK1_PIO1_PIN21_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN21_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_22
   type SEC_GPIO_MASK1_PIO1_PIN22_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN22_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_23
   type SEC_GPIO_MASK1_PIO1_PIN23_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN23_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_24
   type SEC_GPIO_MASK1_PIO1_PIN24_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN24_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_25
   type SEC_GPIO_MASK1_PIO1_PIN25_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN25_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_26
   type SEC_GPIO_MASK1_PIO1_PIN26_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN26_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_27
   type SEC_GPIO_MASK1_PIO1_PIN27_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN27_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_28
   type SEC_GPIO_MASK1_PIO1_PIN28_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN28_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_29
   type SEC_GPIO_MASK1_PIO1_PIN29_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN29_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_30
   type SEC_GPIO_MASK1_PIO1_PIN30_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN30_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure mask for pin P1_31
   type SEC_GPIO_MASK1_PIO1_PIN31_SEC_MASK_Field is
     (
      --  Pin state is blocked to non-secure world.
      Blocked,
      --  Pin state is readable by non-secure world.
      Readable)
     with Size => 1;
   for SEC_GPIO_MASK1_PIO1_PIN31_SEC_MASK_Field use
     (Blocked => 0,
      Readable => 1);

   --  Secure GPIO mask for port 1 pins.
   type SEC_GPIO_MASK1_Register is record
      --  Secure mask for pin P1_0
      PIO1_PIN0_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN0_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_1
      PIO1_PIN1_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN1_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_2
      PIO1_PIN2_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN2_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_3
      PIO1_PIN3_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN3_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_4
      PIO1_PIN4_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN4_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_5
      PIO1_PIN5_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN5_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_6
      PIO1_PIN6_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN6_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_7
      PIO1_PIN7_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN7_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_8
      PIO1_PIN8_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN8_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_9
      PIO1_PIN9_SEC_MASK  : SEC_GPIO_MASK1_PIO1_PIN9_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_10
      PIO1_PIN10_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN10_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_11
      PIO1_PIN11_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN11_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_12
      PIO1_PIN12_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN12_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_13
      PIO1_PIN13_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN13_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_14
      PIO1_PIN14_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN14_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_15
      PIO1_PIN15_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN15_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_16
      PIO1_PIN16_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN16_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_17
      PIO1_PIN17_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN17_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_18
      PIO1_PIN18_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN18_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_19
      PIO1_PIN19_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN19_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_20
      PIO1_PIN20_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN20_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_21
      PIO1_PIN21_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN21_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_22
      PIO1_PIN22_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN22_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_23
      PIO1_PIN23_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN23_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_24
      PIO1_PIN24_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN24_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_25
      PIO1_PIN25_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN25_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_26
      PIO1_PIN26_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN26_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_27
      PIO1_PIN27_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN27_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_28
      PIO1_PIN28_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN28_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_29
      PIO1_PIN29_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN29_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_30
      PIO1_PIN30_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN30_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
      --  Secure mask for pin P1_31
      PIO1_PIN31_SEC_MASK : SEC_GPIO_MASK1_PIO1_PIN31_SEC_MASK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Readable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_GPIO_MASK1_Register use record
      PIO1_PIN0_SEC_MASK  at 0 range 0 .. 0;
      PIO1_PIN1_SEC_MASK  at 0 range 1 .. 1;
      PIO1_PIN2_SEC_MASK  at 0 range 2 .. 2;
      PIO1_PIN3_SEC_MASK  at 0 range 3 .. 3;
      PIO1_PIN4_SEC_MASK  at 0 range 4 .. 4;
      PIO1_PIN5_SEC_MASK  at 0 range 5 .. 5;
      PIO1_PIN6_SEC_MASK  at 0 range 6 .. 6;
      PIO1_PIN7_SEC_MASK  at 0 range 7 .. 7;
      PIO1_PIN8_SEC_MASK  at 0 range 8 .. 8;
      PIO1_PIN9_SEC_MASK  at 0 range 9 .. 9;
      PIO1_PIN10_SEC_MASK at 0 range 10 .. 10;
      PIO1_PIN11_SEC_MASK at 0 range 11 .. 11;
      PIO1_PIN12_SEC_MASK at 0 range 12 .. 12;
      PIO1_PIN13_SEC_MASK at 0 range 13 .. 13;
      PIO1_PIN14_SEC_MASK at 0 range 14 .. 14;
      PIO1_PIN15_SEC_MASK at 0 range 15 .. 15;
      PIO1_PIN16_SEC_MASK at 0 range 16 .. 16;
      PIO1_PIN17_SEC_MASK at 0 range 17 .. 17;
      PIO1_PIN18_SEC_MASK at 0 range 18 .. 18;
      PIO1_PIN19_SEC_MASK at 0 range 19 .. 19;
      PIO1_PIN20_SEC_MASK at 0 range 20 .. 20;
      PIO1_PIN21_SEC_MASK at 0 range 21 .. 21;
      PIO1_PIN22_SEC_MASK at 0 range 22 .. 22;
      PIO1_PIN23_SEC_MASK at 0 range 23 .. 23;
      PIO1_PIN24_SEC_MASK at 0 range 24 .. 24;
      PIO1_PIN25_SEC_MASK at 0 range 25 .. 25;
      PIO1_PIN26_SEC_MASK at 0 range 26 .. 26;
      PIO1_PIN27_SEC_MASK at 0 range 27 .. 27;
      PIO1_PIN28_SEC_MASK at 0 range 28 .. 28;
      PIO1_PIN29_SEC_MASK at 0 range 29 .. 29;
      PIO1_PIN30_SEC_MASK at 0 range 30 .. 30;
      PIO1_PIN31_SEC_MASK at 0 range 31 .. 31;
   end record;

   --  Watchdog Timer, Brown Out Detectors and Flash Controller interrupts
   type SEC_CPU_INT_MASK0_SYS_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_SYS_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  System DMA 0 (non-secure) interrupt.
   type SEC_CPU_INT_MASK0_SDMA0_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_SDMA0_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  GPIO Group 0 interrupt.
   type SEC_CPU_INT_MASK0_GPIO_GLOBALINT0_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_GPIO_GLOBALINT0_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  GPIO Group 1 interrupt.
   type SEC_CPU_INT_MASK0_GPIO_GLOBALINT1_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_GPIO_GLOBALINT1_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Pin interrupt 0 or pattern match engine slice 0 interrupt.
   type SEC_CPU_INT_MASK0_GPIO_INT0_IRQ0_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_GPIO_INT0_IRQ0_Field use
     (Invisible => 0,
      Visible => 1);

   --  SEC_CPU_INT_MASK0_GPIO_INT0_IRQ array
   type SEC_CPU_INT_MASK0_GPIO_INT0_IRQ_Field_Array is array (0 .. 3)
     of SEC_CPU_INT_MASK0_GPIO_INT0_IRQ0_Field
     with Component_Size => 1, Size => 4;

   --  Type definition for SEC_CPU_INT_MASK0_GPIO_INT0_IRQ
   type SEC_CPU_INT_MASK0_GPIO_INT0_IRQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GPIO_INT0_IRQ as a value
            Val : HAL.UInt4;
         when True =>
            --  GPIO_INT0_IRQ as an array
            Arr : SEC_CPU_INT_MASK0_GPIO_INT0_IRQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SEC_CPU_INT_MASK0_GPIO_INT0_IRQ_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Micro Tick Timer interrupt.
   type SEC_CPU_INT_MASK0_UTICK_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_UTICK_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Multi-Rate Timer interrupt.
   type SEC_CPU_INT_MASK0_MRT_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_MRT_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Standard counter/timer 0 interrupt.
   type SEC_CPU_INT_MASK0_CTIMER0_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_CTIMER0_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Standard counter/timer 1 interrupt.
   type SEC_CPU_INT_MASK0_CTIMER1_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_CTIMER1_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  SCTimer/PWM interrupt.
   type SEC_CPU_INT_MASK0_SCT_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_SCT_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Standard counter/timer 3 interrupt.
   type SEC_CPU_INT_MASK0_CTIMER3_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_CTIMER3_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Flexcomm 0 interrupt (USART, SPI, I2C, I2S).
   type SEC_CPU_INT_MASK0_FLEXCOMM0_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_FLEXCOMM0_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Flexcomm 1 interrupt (USART, SPI, I2C, I2S).
   type SEC_CPU_INT_MASK0_FLEXCOMM1_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_FLEXCOMM1_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Flexcomm 2 interrupt (USART, SPI, I2C, I2S).
   type SEC_CPU_INT_MASK0_FLEXCOMM2_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_FLEXCOMM2_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Flexcomm 3 interrupt (USART, SPI, I2C, I2S).
   type SEC_CPU_INT_MASK0_FLEXCOMM3_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_FLEXCOMM3_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Flexcomm 4 interrupt (USART, SPI, I2C, I2S).
   type SEC_CPU_INT_MASK0_FLEXCOMM4_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_FLEXCOMM4_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Flexcomm 5 interrupt (USART, SPI, I2C, I2S).
   type SEC_CPU_INT_MASK0_FLEXCOMM5_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_FLEXCOMM5_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Flexcomm 6 interrupt (USART, SPI, I2C, I2S).
   type SEC_CPU_INT_MASK0_FLEXCOMM6_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_FLEXCOMM6_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Flexcomm 7 interrupt (USART, SPI, I2C, I2S).
   type SEC_CPU_INT_MASK0_FLEXCOMM7_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_FLEXCOMM7_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  General Purpose ADC interrupt.
   type SEC_CPU_INT_MASK0_ADC_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_ADC_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Reserved. Read value is undefined, only zero should be written.
   type SEC_CPU_INT_MASK0_RESERVED0_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_RESERVED0_Field use
     (Invisible => 0,
      Visible => 1);

   --  Analog Comparator interrupt.
   type SEC_CPU_INT_MASK0_ACMP_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_ACMP_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Reserved. Read value is undefined, only zero should be written.
   type SEC_CPU_INT_MASK0_RESERVED1_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_RESERVED1_Field use
     (Invisible => 0,
      Visible => 1);

   --  SEC_CPU_INT_MASK0_RESERVED array
   type SEC_CPU_INT_MASK0_RESERVED_Field_Array is array (1 .. 2)
     of SEC_CPU_INT_MASK0_RESERVED1_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for SEC_CPU_INT_MASK0_RESERVED
   type SEC_CPU_INT_MASK0_RESERVED_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RESERVED as a value
            Val : HAL.UInt2;
         when True =>
            --  RESERVED as an array
            Arr : SEC_CPU_INT_MASK0_RESERVED_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SEC_CPU_INT_MASK0_RESERVED_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  USB Full Speed Controller Clock request interrupt.
   type SEC_CPU_INT_MASK0_USB0_NEEDCLK_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_USB0_NEEDCLK_Field use
     (Invisible => 0,
      Visible => 1);

   --  USB Full Speed Controller interrupt.
   type SEC_CPU_INT_MASK0_USB0_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_USB0_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  RTC_LITE0_ALARM_IRQ, RTC_LITE0_WAKEUP_IRQ
   type SEC_CPU_INT_MASK0_RTC_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_RTC_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Reserved. Read value is undefined, only zero should be written.
   type SEC_CPU_INT_MASK0_RESERVED3_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_RESERVED3_Field use
     (Invisible => 0,
      Visible => 1);

   --  Mailbox interrupt.
   type SEC_CPU_INT_MASK0_MAILBOX_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK0_MAILBOX_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Secure Interrupt mask for CPU1
   type SEC_CPU_INT_MASK0_Register is record
      --  Watchdog Timer, Brown Out Detectors and Flash Controller interrupts
      SYS_IRQ             : SEC_CPU_INT_MASK0_SYS_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  System DMA 0 (non-secure) interrupt.
      SDMA0_IRQ           : SEC_CPU_INT_MASK0_SDMA0_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  GPIO Group 0 interrupt.
      GPIO_GLOBALINT0_IRQ : SEC_CPU_INT_MASK0_GPIO_GLOBALINT0_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  GPIO Group 1 interrupt.
      GPIO_GLOBALINT1_IRQ : SEC_CPU_INT_MASK0_GPIO_GLOBALINT1_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Pin interrupt 0 or pattern match engine slice 0 interrupt.
      GPIO_INT0_IRQ       : SEC_CPU_INT_MASK0_GPIO_INT0_IRQ_Field :=
                             (As_Array => False, Val => 16#1#);
      --  Micro Tick Timer interrupt.
      UTICK_IRQ           : SEC_CPU_INT_MASK0_UTICK_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Multi-Rate Timer interrupt.
      MRT_IRQ             : SEC_CPU_INT_MASK0_MRT_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Standard counter/timer 0 interrupt.
      CTIMER0_IRQ         : SEC_CPU_INT_MASK0_CTIMER0_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Standard counter/timer 1 interrupt.
      CTIMER1_IRQ         : SEC_CPU_INT_MASK0_CTIMER1_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  SCTimer/PWM interrupt.
      SCT_IRQ             : SEC_CPU_INT_MASK0_SCT_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Standard counter/timer 3 interrupt.
      CTIMER3_IRQ         : SEC_CPU_INT_MASK0_CTIMER3_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Flexcomm 0 interrupt (USART, SPI, I2C, I2S).
      FLEXCOMM0_IRQ       : SEC_CPU_INT_MASK0_FLEXCOMM0_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Flexcomm 1 interrupt (USART, SPI, I2C, I2S).
      FLEXCOMM1_IRQ       : SEC_CPU_INT_MASK0_FLEXCOMM1_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Flexcomm 2 interrupt (USART, SPI, I2C, I2S).
      FLEXCOMM2_IRQ       : SEC_CPU_INT_MASK0_FLEXCOMM2_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Flexcomm 3 interrupt (USART, SPI, I2C, I2S).
      FLEXCOMM3_IRQ       : SEC_CPU_INT_MASK0_FLEXCOMM3_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Flexcomm 4 interrupt (USART, SPI, I2C, I2S).
      FLEXCOMM4_IRQ       : SEC_CPU_INT_MASK0_FLEXCOMM4_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Flexcomm 5 interrupt (USART, SPI, I2C, I2S).
      FLEXCOMM5_IRQ       : SEC_CPU_INT_MASK0_FLEXCOMM5_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Flexcomm 6 interrupt (USART, SPI, I2C, I2S).
      FLEXCOMM6_IRQ       : SEC_CPU_INT_MASK0_FLEXCOMM6_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Flexcomm 7 interrupt (USART, SPI, I2C, I2S).
      FLEXCOMM7_IRQ       : SEC_CPU_INT_MASK0_FLEXCOMM7_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  General Purpose ADC interrupt.
      ADC_IRQ             : SEC_CPU_INT_MASK0_ADC_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Reserved. Read value is undefined, only zero should be written.
      RESERVED0           : SEC_CPU_INT_MASK0_RESERVED0_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Analog Comparator interrupt.
      ACMP_IRQ            : SEC_CPU_INT_MASK0_ACMP_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Reserved. Read value is undefined, only zero should be written.
      RESERVED            : SEC_CPU_INT_MASK0_RESERVED_Field :=
                             (As_Array => False, Val => 16#1#);
      --  USB Full Speed Controller Clock request interrupt.
      USB0_NEEDCLK        : SEC_CPU_INT_MASK0_USB0_NEEDCLK_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  USB Full Speed Controller interrupt.
      USB0_IRQ            : SEC_CPU_INT_MASK0_USB0_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  RTC_LITE0_ALARM_IRQ, RTC_LITE0_WAKEUP_IRQ
      RTC_IRQ             : SEC_CPU_INT_MASK0_RTC_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Reserved. Read value is undefined, only zero should be written.
      RESERVED3           : SEC_CPU_INT_MASK0_RESERVED3_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Mailbox interrupt.
      MAILBOX_IRQ         : SEC_CPU_INT_MASK0_MAILBOX_IRQ_Field :=
                             NXP_SVD.AHB_SECURE_CTRL.Visible;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CPU_INT_MASK0_Register use record
      SYS_IRQ             at 0 range 0 .. 0;
      SDMA0_IRQ           at 0 range 1 .. 1;
      GPIO_GLOBALINT0_IRQ at 0 range 2 .. 2;
      GPIO_GLOBALINT1_IRQ at 0 range 3 .. 3;
      GPIO_INT0_IRQ       at 0 range 4 .. 7;
      UTICK_IRQ           at 0 range 8 .. 8;
      MRT_IRQ             at 0 range 9 .. 9;
      CTIMER0_IRQ         at 0 range 10 .. 10;
      CTIMER1_IRQ         at 0 range 11 .. 11;
      SCT_IRQ             at 0 range 12 .. 12;
      CTIMER3_IRQ         at 0 range 13 .. 13;
      FLEXCOMM0_IRQ       at 0 range 14 .. 14;
      FLEXCOMM1_IRQ       at 0 range 15 .. 15;
      FLEXCOMM2_IRQ       at 0 range 16 .. 16;
      FLEXCOMM3_IRQ       at 0 range 17 .. 17;
      FLEXCOMM4_IRQ       at 0 range 18 .. 18;
      FLEXCOMM5_IRQ       at 0 range 19 .. 19;
      FLEXCOMM6_IRQ       at 0 range 20 .. 20;
      FLEXCOMM7_IRQ       at 0 range 21 .. 21;
      ADC_IRQ             at 0 range 22 .. 22;
      RESERVED0           at 0 range 23 .. 23;
      ACMP_IRQ            at 0 range 24 .. 24;
      RESERVED            at 0 range 25 .. 26;
      USB0_NEEDCLK        at 0 range 27 .. 27;
      USB0_IRQ            at 0 range 28 .. 28;
      RTC_IRQ             at 0 range 29 .. 29;
      RESERVED3           at 0 range 30 .. 30;
      MAILBOX_IRQ         at 0 range 31 .. 31;
   end record;

   --  Pin interrupt 4 or pattern match engine slice 4 interrupt.
   type SEC_CPU_INT_MASK1_GPIO_INT0_IRQ4_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_GPIO_INT0_IRQ4_Field use
     (Invisible => 0,
      Visible => 1);

   --  SEC_CPU_INT_MASK1_GPIO_INT0_IRQ array
   type SEC_CPU_INT_MASK1_GPIO_INT0_IRQ_Field_Array is array (4 .. 7)
     of SEC_CPU_INT_MASK1_GPIO_INT0_IRQ4_Field
     with Component_Size => 1, Size => 4;

   --  Type definition for SEC_CPU_INT_MASK1_GPIO_INT0_IRQ
   type SEC_CPU_INT_MASK1_GPIO_INT0_IRQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GPIO_INT0_IRQ as a value
            Val : HAL.UInt4;
         when True =>
            --  GPIO_INT0_IRQ as an array
            Arr : SEC_CPU_INT_MASK1_GPIO_INT0_IRQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SEC_CPU_INT_MASK1_GPIO_INT0_IRQ_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Standard counter/timer 2 interrupt.
   type SEC_CPU_INT_MASK1_CTIMER2_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_CTIMER2_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Standard counter/timer 4 interrupt.
   type SEC_CPU_INT_MASK1_CTIMER4_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_CTIMER4_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  OS Event Timer and OS Event Timer Wakeup interrupts
   type SEC_CPU_INT_MASK1_OS_EVENT_TIMER_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_OS_EVENT_TIMER_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Reserved. Read value is undefined, only zero should be written.
   type SEC_CPU_INT_MASK1_RESERVED0_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_RESERVED0_Field use
     (Invisible => 0,
      Visible => 1);

   --  SEC_CPU_INT_MASK1_RESERVED array
   type SEC_CPU_INT_MASK1_RESERVED_Field_Array is array (0 .. 2)
     of SEC_CPU_INT_MASK1_RESERVED0_Field
     with Component_Size => 1, Size => 3;

   --  Type definition for SEC_CPU_INT_MASK1_RESERVED
   type SEC_CPU_INT_MASK1_RESERVED_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RESERVED as a value
            Val : HAL.UInt3;
         when True =>
            --  RESERVED as an array
            Arr : SEC_CPU_INT_MASK1_RESERVED_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for SEC_CPU_INT_MASK1_RESERVED_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  SDIO Controller interrupt.
   type SEC_CPU_INT_MASK1_SDIO_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_SDIO_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Reserved. Read value is undefined, only zero should be written.
   type SEC_CPU_INT_MASK1_RESERVED3_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_RESERVED3_Field use
     (Invisible => 0,
      Visible => 1);

   --  SEC_CPU_INT_MASK1_RESERVED array
   type SEC_CPU_INT_MASK1_RESERVED_Field_Array_1 is array (3 .. 5)
     of SEC_CPU_INT_MASK1_RESERVED3_Field
     with Component_Size => 1, Size => 3;

   --  Type definition for SEC_CPU_INT_MASK1_RESERVED
   type SEC_CPU_INT_MASK1_RESERVED_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RESERVED as a value
            Val : HAL.UInt3;
         when True =>
            --  RESERVED as an array
            Arr : SEC_CPU_INT_MASK1_RESERVED_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for SEC_CPU_INT_MASK1_RESERVED_Field_1 use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  USB High Speed PHY Controller interrupt.
   type SEC_CPU_INT_MASK1_USB1_PHY_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_USB1_PHY_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  USB High Speed Controller interrupt.
   type SEC_CPU_INT_MASK1_USB1_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_USB1_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  USB High Speed Controller Clock request interrupt.
   type SEC_CPU_INT_MASK1_USB1_NEEDCLK_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_USB1_NEEDCLK_Field use
     (Invisible => 0,
      Visible => 1);

   --  Secure fault Hyper Visor call interrupt.
   type SEC_CPU_INT_MASK1_SEC_HYPERVISOR_CALL_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_SEC_HYPERVISOR_CALL_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Secure Pin interrupt 0 or pattern match engine slice 0 interrupt.
   type SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ0_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ0_Field use
     (Invisible => 0,
      Visible => 1);

   --  SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ array
   type SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ_Field_Array is array (0 .. 1)
     of SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ0_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ
   type SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC_GPIO_INT0_IRQ as a value
            Val : HAL.UInt2;
         when True =>
            --  SEC_GPIO_INT0_IRQ as an array
            Arr : SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Programmable Look-Up Controller interrupt.
   type SEC_CPU_INT_MASK1_PLU_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_PLU_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Security Violation interrupt.
   type SEC_CPU_INT_MASK1_SEC_VIO_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_SEC_VIO_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  HASH-AES interrupt.
   type SEC_CPU_INT_MASK1_SHA_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_SHA_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  CASPER interrupt.
   type SEC_CPU_INT_MASK1_CASPER_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_CASPER_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  PUF interrupt.
   type SEC_CPU_INT_MASK1_PUFKEY_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_PUFKEY_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Power Quad interrupt.
   type SEC_CPU_INT_MASK1_PQ_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_PQ_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  System DMA 1 (Secure) interrupt
   type SEC_CPU_INT_MASK1_SDMA1_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_SDMA1_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  High Speed SPI interrupt
   type SEC_CPU_INT_MASK1_LSPI_HS_IRQ_Field is
     (
      --  no description available
      Invisible,
      --  no description available
      Visible)
     with Size => 1;
   for SEC_CPU_INT_MASK1_LSPI_HS_IRQ_Field use
     (Invisible => 0,
      Visible => 1);

   --  Secure Interrupt mask for CPU1
   type SEC_CPU_INT_MASK1_Register is record
      --  Pin interrupt 4 or pattern match engine slice 4 interrupt.
      GPIO_INT0_IRQ           : SEC_CPU_INT_MASK1_GPIO_INT0_IRQ_Field :=
                                 (As_Array => False, Val => 16#1#);
      --  Standard counter/timer 2 interrupt.
      CTIMER2_IRQ             : SEC_CPU_INT_MASK1_CTIMER2_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Standard counter/timer 4 interrupt.
      CTIMER4_IRQ             : SEC_CPU_INT_MASK1_CTIMER4_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  OS Event Timer and OS Event Timer Wakeup interrupts
      OS_EVENT_TIMER_IRQ      : SEC_CPU_INT_MASK1_OS_EVENT_TIMER_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Reserved. Read value is undefined, only zero should be written.
      RESERVED                : SEC_CPU_INT_MASK1_RESERVED_Field :=
                                 (As_Array => False, Val => 16#1#);
      --  SDIO Controller interrupt.
      SDIO_IRQ                : SEC_CPU_INT_MASK1_SDIO_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Reserved. Read value is undefined, only zero should be written.
      RESERVED_1              : SEC_CPU_INT_MASK1_RESERVED_Field_1 :=
                                 (As_Array => False, Val => 16#1#);
      --  USB High Speed PHY Controller interrupt.
      USB1_PHY_IRQ            : SEC_CPU_INT_MASK1_USB1_PHY_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  USB High Speed Controller interrupt.
      USB1_IRQ                : SEC_CPU_INT_MASK1_USB1_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  USB High Speed Controller Clock request interrupt.
      USB1_NEEDCLK            : SEC_CPU_INT_MASK1_USB1_NEEDCLK_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Secure fault Hyper Visor call interrupt.
      SEC_HYPERVISOR_CALL_IRQ : SEC_CPU_INT_MASK1_SEC_HYPERVISOR_CALL_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Secure Pin interrupt 0 or pattern match engine slice 0 interrupt.
      SEC_GPIO_INT0_IRQ       : SEC_CPU_INT_MASK1_SEC_GPIO_INT0_IRQ_Field :=
                                 (As_Array => False, Val => 16#1#);
      --  Programmable Look-Up Controller interrupt.
      PLU_IRQ                 : SEC_CPU_INT_MASK1_PLU_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Security Violation interrupt.
      SEC_VIO_IRQ             : SEC_CPU_INT_MASK1_SEC_VIO_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  HASH-AES interrupt.
      SHA_IRQ                 : SEC_CPU_INT_MASK1_SHA_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  CASPER interrupt.
      CASPER_IRQ              : SEC_CPU_INT_MASK1_CASPER_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  PUF interrupt.
      PUFKEY_IRQ              : SEC_CPU_INT_MASK1_PUFKEY_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  Power Quad interrupt.
      PQ_IRQ                  : SEC_CPU_INT_MASK1_PQ_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  System DMA 1 (Secure) interrupt
      SDMA1_IRQ               : SEC_CPU_INT_MASK1_SDMA1_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  High Speed SPI interrupt
      LSPI_HS_IRQ             : SEC_CPU_INT_MASK1_LSPI_HS_IRQ_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Visible;
      --  unspecified
      Reserved_28_31          : HAL.UInt4 := 16#F#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_CPU_INT_MASK1_Register use record
      GPIO_INT0_IRQ           at 0 range 0 .. 3;
      CTIMER2_IRQ             at 0 range 4 .. 4;
      CTIMER4_IRQ             at 0 range 5 .. 5;
      OS_EVENT_TIMER_IRQ      at 0 range 6 .. 6;
      RESERVED                at 0 range 7 .. 9;
      SDIO_IRQ                at 0 range 10 .. 10;
      RESERVED_1              at 0 range 11 .. 13;
      USB1_PHY_IRQ            at 0 range 14 .. 14;
      USB1_IRQ                at 0 range 15 .. 15;
      USB1_NEEDCLK            at 0 range 16 .. 16;
      SEC_HYPERVISOR_CALL_IRQ at 0 range 17 .. 17;
      SEC_GPIO_INT0_IRQ       at 0 range 18 .. 19;
      PLU_IRQ                 at 0 range 20 .. 20;
      SEC_VIO_IRQ             at 0 range 21 .. 21;
      SHA_IRQ                 at 0 range 22 .. 22;
      CASPER_IRQ              at 0 range 23 .. 23;
      PUFKEY_IRQ              at 0 range 24 .. 24;
      PQ_IRQ                  at 0 range 25 .. 25;
      SDMA1_IRQ               at 0 range 26 .. 26;
      LSPI_HS_IRQ             at 0 range 27 .. 27;
      Reserved_28_31          at 0 range 28 .. 31;
   end record;

   --  SEC_GPIO_MASK0 register write-lock.
   type SEC_MASK_LOCK_SEC_GPIO_MASK0_LOCK_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for SEC_MASK_LOCK_SEC_GPIO_MASK0_LOCK_Field use
     (Blocked => 1,
      Writable => 2);

   --  SEC_GPIO_MASK1 register write-lock.
   type SEC_MASK_LOCK_SEC_GPIO_MASK1_LOCK_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for SEC_MASK_LOCK_SEC_GPIO_MASK1_LOCK_Field use
     (Blocked => 1,
      Writable => 2);

   --  SEC_CPU_INT_MASK0 register write-lock.
   type SEC_MASK_LOCK_SEC_CPU1_INT_MASK0_LOCK_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for SEC_MASK_LOCK_SEC_CPU1_INT_MASK0_LOCK_Field use
     (Blocked => 1,
      Writable => 2);

   --  SEC_CPU_INT_MASK1 register write-lock.
   type SEC_MASK_LOCK_SEC_CPU1_INT_MASK1_LOCK_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for SEC_MASK_LOCK_SEC_CPU1_INT_MASK1_LOCK_Field use
     (Blocked => 1,
      Writable => 2);

   --  Security General Purpose register access control.
   type SEC_MASK_LOCK_Register is record
      --  SEC_GPIO_MASK0 register write-lock.
      SEC_GPIO_MASK0_LOCK     : SEC_MASK_LOCK_SEC_GPIO_MASK0_LOCK_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  SEC_GPIO_MASK1 register write-lock.
      SEC_GPIO_MASK1_LOCK     : SEC_MASK_LOCK_SEC_GPIO_MASK1_LOCK_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  unspecified
      Reserved_4_7            : HAL.UInt4 := 16#A#;
      --  SEC_CPU_INT_MASK0 register write-lock.
      SEC_CPU1_INT_MASK0_LOCK : SEC_MASK_LOCK_SEC_CPU1_INT_MASK0_LOCK_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  SEC_CPU_INT_MASK1 register write-lock.
      SEC_CPU1_INT_MASK1_LOCK : SEC_MASK_LOCK_SEC_CPU1_INT_MASK1_LOCK_Field :=
                                 NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  unspecified
      Reserved_12_31          : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEC_MASK_LOCK_Register use record
      SEC_GPIO_MASK0_LOCK     at 0 range 0 .. 1;
      SEC_GPIO_MASK1_LOCK     at 0 range 2 .. 3;
      Reserved_4_7            at 0 range 4 .. 7;
      SEC_CPU1_INT_MASK0_LOCK at 0 range 8 .. 9;
      SEC_CPU1_INT_MASK1_LOCK at 0 range 10 .. 11;
      Reserved_12_31          at 0 range 12 .. 31;
   end record;

   --  Micro-Cortex M33 (CPU1) Code bus.
   type MASTER_SEC_LEVEL_CPU1C_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for MASTER_SEC_LEVEL_CPU1C_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Micro-Cortex M33 (CPU1) System bus.
   type MASTER_SEC_LEVEL_CPU1S_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for MASTER_SEC_LEVEL_CPU1S_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  USB Full Speed Device.
   type MASTER_SEC_LEVEL_USBFSD_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for MASTER_SEC_LEVEL_USBFSD_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  System DMA 0.
   type MASTER_SEC_LEVEL_SDMA0_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for MASTER_SEC_LEVEL_SDMA0_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  SDIO.
   type MASTER_SEC_LEVEL_SDIO_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for MASTER_SEC_LEVEL_SDIO_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Power Quad.
   type MASTER_SEC_LEVEL_PQ_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for MASTER_SEC_LEVEL_PQ_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  Hash.
   type MASTER_SEC_LEVEL_HASH_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for MASTER_SEC_LEVEL_HASH_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  USB Full speed Host.
   type MASTER_SEC_LEVEL_USBFSH_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for MASTER_SEC_LEVEL_USBFSH_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  System DMA 1 security level.
   type MASTER_SEC_LEVEL_SDMA1_Field is
     (
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Secure and Priviledge user access allowed.
      Enum_S_P)
     with Size => 2;
   for MASTER_SEC_LEVEL_SDMA1_Field use
     (Enum_Ns_Np => 0,
      Enum_Ns_P => 1,
      Enum_S_Np => 2,
      Enum_S_P => 3);

   --  MASTER_SEC_LEVEL write-lock.
   type MASTER_SEC_LEVEL_MASTER_SEC_LEVEL_LOCK_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for MASTER_SEC_LEVEL_MASTER_SEC_LEVEL_LOCK_Field use
     (Blocked => 1,
      Writable => 2);

   --  master secure level register
   type MASTER_SEC_LEVEL_Register is record
      --  unspecified
      Reserved_0_3          : HAL.UInt4 := 16#0#;
      --  Micro-Cortex M33 (CPU1) Code bus.
      CPU1C                 : MASTER_SEC_LEVEL_CPU1C_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  Micro-Cortex M33 (CPU1) System bus.
      CPU1S                 : MASTER_SEC_LEVEL_CPU1S_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  USB Full Speed Device.
      USBFSD                : MASTER_SEC_LEVEL_USBFSD_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  System DMA 0.
      SDMA0                 : MASTER_SEC_LEVEL_SDMA0_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_12_15        : HAL.UInt4 := 16#0#;
      --  SDIO.
      SDIO                  : MASTER_SEC_LEVEL_SDIO_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  Power Quad.
      PQ                    : MASTER_SEC_LEVEL_PQ_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  Hash.
      HASH                  : MASTER_SEC_LEVEL_HASH_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  USB Full speed Host.
      USBFSH                : MASTER_SEC_LEVEL_USBFSH_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  System DMA 1 security level.
      SDMA1                 : MASTER_SEC_LEVEL_SDMA1_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_29        : HAL.UInt4 := 16#0#;
      --  MASTER_SEC_LEVEL write-lock.
      MASTER_SEC_LEVEL_LOCK : MASTER_SEC_LEVEL_MASTER_SEC_LEVEL_LOCK_Field :=
                               NXP_SVD.AHB_SECURE_CTRL.Writable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MASTER_SEC_LEVEL_Register use record
      Reserved_0_3          at 0 range 0 .. 3;
      CPU1C                 at 0 range 4 .. 5;
      CPU1S                 at 0 range 6 .. 7;
      USBFSD                at 0 range 8 .. 9;
      SDMA0                 at 0 range 10 .. 11;
      Reserved_12_15        at 0 range 12 .. 15;
      SDIO                  at 0 range 16 .. 17;
      PQ                    at 0 range 18 .. 19;
      HASH                  at 0 range 20 .. 21;
      USBFSH                at 0 range 22 .. 23;
      SDMA1                 at 0 range 24 .. 25;
      Reserved_26_29        at 0 range 26 .. 29;
      MASTER_SEC_LEVEL_LOCK at 0 range 30 .. 31;
   end record;

   --  Micro-Cortex M33 (CPU1) Code bus. Must be equal to
   --  NOT(MASTER_SEC_LEVEL.CPU1C)
   type MASTER_SEC_ANTI_POL_REG_CPU1C_Field is
     (
      --  Secure and Priviledge user access allowed.
      Enum_S_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_CPU1C_Field use
     (Enum_S_P => 0,
      Enum_S_Np => 1,
      Enum_Ns_P => 2,
      Enum_Ns_Np => 3);

   --  Micro-Cortex M33 (CPU1) System bus. Must be equal to
   --  NOT(MASTER_SEC_LEVEL.CPU1S)
   type MASTER_SEC_ANTI_POL_REG_CPU1S_Field is
     (
      --  Secure and Priviledge user access allowed.
      Enum_S_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_CPU1S_Field use
     (Enum_S_P => 0,
      Enum_S_Np => 1,
      Enum_Ns_P => 2,
      Enum_Ns_Np => 3);

   --  USB Full Speed Device. Must be equal to NOT(MASTER_SEC_LEVEL.USBFSD)
   type MASTER_SEC_ANTI_POL_REG_USBFSD_Field is
     (
      --  Secure and Priviledge user access allowed.
      Enum_S_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_USBFSD_Field use
     (Enum_S_P => 0,
      Enum_S_Np => 1,
      Enum_Ns_P => 2,
      Enum_Ns_Np => 3);

   --  System DMA 0. Must be equal to NOT(MASTER_SEC_LEVEL.SDMA0)
   type MASTER_SEC_ANTI_POL_REG_SDMA0_Field is
     (
      --  Secure and Priviledge user access allowed.
      Enum_S_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_SDMA0_Field use
     (Enum_S_P => 0,
      Enum_S_Np => 1,
      Enum_Ns_P => 2,
      Enum_Ns_Np => 3);

   --  SDIO. Must be equal to NOT(MASTER_SEC_LEVEL.SDIO)
   type MASTER_SEC_ANTI_POL_REG_SDIO_Field is
     (
      --  Secure and Priviledge user access allowed.
      Enum_S_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_SDIO_Field use
     (Enum_S_P => 0,
      Enum_S_Np => 1,
      Enum_Ns_P => 2,
      Enum_Ns_Np => 3);

   --  Power Quad. Must be equal to NOT(MASTER_SEC_LEVEL.PQ)
   type MASTER_SEC_ANTI_POL_REG_PQ_Field is
     (
      --  Secure and Priviledge user access allowed.
      Enum_S_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_PQ_Field use
     (Enum_S_P => 0,
      Enum_S_Np => 1,
      Enum_Ns_P => 2,
      Enum_Ns_Np => 3);

   --  Hash. Must be equal to NOT(MASTER_SEC_LEVEL.HASH)
   type MASTER_SEC_ANTI_POL_REG_HASH_Field is
     (
      --  Secure and Priviledge user access allowed.
      Enum_S_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_HASH_Field use
     (Enum_S_P => 0,
      Enum_S_Np => 1,
      Enum_Ns_P => 2,
      Enum_Ns_Np => 3);

   --  USB Full speed Host. Must be equal to NOT(MASTER_SEC_LEVEL.USBFSH)
   type MASTER_SEC_ANTI_POL_REG_USBFSH_Field is
     (
      --  Secure and Priviledge user access allowed.
      Enum_S_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_USBFSH_Field use
     (Enum_S_P => 0,
      Enum_S_Np => 1,
      Enum_Ns_P => 2,
      Enum_Ns_Np => 3);

   --  System DMA 1 security level. Must be equal to
   --  NOT(MASTER_SEC_LEVEL.SDMA1)
   type MASTER_SEC_ANTI_POL_REG_SDMA1_Field is
     (
      --  Secure and Priviledge user access allowed.
      Enum_S_P,
      --  Secure and Non-priviledge user access allowed.
      Enum_S_Np,
      --  Non-secure and Privilege access allowed.
      Enum_Ns_P,
      --  Non-secure and Non-priviledge user access allowed.
      Enum_Ns_Np)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_SDMA1_Field use
     (Enum_S_P => 0,
      Enum_S_Np => 1,
      Enum_Ns_P => 2,
      Enum_Ns_Np => 3);

   --  MASTER_SEC_ANTI_POL_REG register write-lock.
   type MASTER_SEC_ANTI_POL_REG_MASTER_SEC_LEVEL_ANTIPOL_LOCK_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for MASTER_SEC_ANTI_POL_REG_MASTER_SEC_LEVEL_ANTIPOL_LOCK_Field use
     (Blocked => 1,
      Writable => 2);

   --  master secure level anti-pole register
   type MASTER_SEC_ANTI_POL_REG_Register is record
      --  unspecified
      Reserved_0_3                  : HAL.UInt4 := 16#F#;
      --  Micro-Cortex M33 (CPU1) Code bus. Must be equal to
      --  NOT(MASTER_SEC_LEVEL.CPU1C)
      CPU1C                         : MASTER_SEC_ANTI_POL_REG_CPU1C_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  Micro-Cortex M33 (CPU1) System bus. Must be equal to
      --  NOT(MASTER_SEC_LEVEL.CPU1S)
      CPU1S                         : MASTER_SEC_ANTI_POL_REG_CPU1S_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  USB Full Speed Device. Must be equal to NOT(MASTER_SEC_LEVEL.USBFSD)
      USBFSD                        : MASTER_SEC_ANTI_POL_REG_USBFSD_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  System DMA 0. Must be equal to NOT(MASTER_SEC_LEVEL.SDMA0)
      SDMA0                         : MASTER_SEC_ANTI_POL_REG_SDMA0_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_12_15                : HAL.UInt4 := 16#F#;
      --  SDIO. Must be equal to NOT(MASTER_SEC_LEVEL.SDIO)
      SDIO                          : MASTER_SEC_ANTI_POL_REG_SDIO_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  Power Quad. Must be equal to NOT(MASTER_SEC_LEVEL.PQ)
      PQ                            : MASTER_SEC_ANTI_POL_REG_PQ_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  Hash. Must be equal to NOT(MASTER_SEC_LEVEL.HASH)
      HASH                          : MASTER_SEC_ANTI_POL_REG_HASH_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  USB Full speed Host. Must be equal to NOT(MASTER_SEC_LEVEL.USBFSH)
      USBFSH                        : MASTER_SEC_ANTI_POL_REG_USBFSH_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  System DMA 1 security level. Must be equal to
      --  NOT(MASTER_SEC_LEVEL.SDMA1)
      SDMA1                         : MASTER_SEC_ANTI_POL_REG_SDMA1_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Enum_Ns_Np;
      --  unspecified
      Reserved_26_29                : HAL.UInt4 := 16#F#;
      --  MASTER_SEC_ANTI_POL_REG register write-lock.
      MASTER_SEC_LEVEL_ANTIPOL_LOCK : MASTER_SEC_ANTI_POL_REG_MASTER_SEC_LEVEL_ANTIPOL_LOCK_Field :=
                                       NXP_SVD.AHB_SECURE_CTRL.Writable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MASTER_SEC_ANTI_POL_REG_Register use record
      Reserved_0_3                  at 0 range 0 .. 3;
      CPU1C                         at 0 range 4 .. 5;
      CPU1S                         at 0 range 6 .. 7;
      USBFSD                        at 0 range 8 .. 9;
      SDMA0                         at 0 range 10 .. 11;
      Reserved_12_15                at 0 range 12 .. 15;
      SDIO                          at 0 range 16 .. 17;
      PQ                            at 0 range 18 .. 19;
      HASH                          at 0 range 20 .. 21;
      USBFSH                        at 0 range 22 .. 23;
      SDMA1                         at 0 range 24 .. 25;
      Reserved_26_29                at 0 range 26 .. 29;
      MASTER_SEC_LEVEL_ANTIPOL_LOCK at 0 range 30 .. 31;
   end record;

   --  Cortex M33 (CPU0) VTOR_NS register write-lock.
   type CPU0_LOCK_REG_LOCK_NS_VTOR_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for CPU0_LOCK_REG_LOCK_NS_VTOR_Field use
     (Blocked => 1,
      Writable => 2);

   --  Cortex M33 (CPU0) non-secure MPU register write-lock.
   type CPU0_LOCK_REG_LOCK_NS_MPU_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for CPU0_LOCK_REG_LOCK_NS_MPU_Field use
     (Blocked => 1,
      Writable => 2);

   --  Cortex M33 (CPU0) VTOR_S, AIRCR.PRIS, IRCR.BFHFNMINS registers
   --  write-lock.
   type CPU0_LOCK_REG_LOCK_S_VTAIRCR_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for CPU0_LOCK_REG_LOCK_S_VTAIRCR_Field use
     (Blocked => 1,
      Writable => 2);

   --  Cortex M33 (CPU0) Secure MPU registers write-lock.
   type CPU0_LOCK_REG_LOCK_S_MPU_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for CPU0_LOCK_REG_LOCK_S_MPU_Field use
     (Blocked => 1,
      Writable => 2);

   --  Cortex M33 (CPU0) SAU registers write-lock.
   type CPU0_LOCK_REG_LOCK_SAU_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for CPU0_LOCK_REG_LOCK_SAU_Field use
     (Blocked => 1,
      Writable => 2);

   --  CPU0_LOCK_REG write-lock.
   type CPU0_LOCK_REG_CPU0_LOCK_REG_LOCK_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for CPU0_LOCK_REG_CPU0_LOCK_REG_LOCK_Field use
     (Blocked => 1,
      Writable => 2);

   --  Miscalleneous control signals for in Cortex M33 (CPU0)
   type CPU0_LOCK_REG_Register is record
      --  Cortex M33 (CPU0) VTOR_NS register write-lock.
      LOCK_NS_VTOR       : CPU0_LOCK_REG_LOCK_NS_VTOR_Field :=
                            NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  Cortex M33 (CPU0) non-secure MPU register write-lock.
      LOCK_NS_MPU        : CPU0_LOCK_REG_LOCK_NS_MPU_Field :=
                            NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  Cortex M33 (CPU0) VTOR_S, AIRCR.PRIS, IRCR.BFHFNMINS registers
      --  write-lock.
      LOCK_S_VTAIRCR     : CPU0_LOCK_REG_LOCK_S_VTAIRCR_Field :=
                            NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  Cortex M33 (CPU0) Secure MPU registers write-lock.
      LOCK_S_MPU         : CPU0_LOCK_REG_LOCK_S_MPU_Field :=
                            NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  Cortex M33 (CPU0) SAU registers write-lock.
      LOCK_SAU           : CPU0_LOCK_REG_LOCK_SAU_Field :=
                            NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  unspecified
      Reserved_10_29     : HAL.UInt20 := 16#0#;
      --  CPU0_LOCK_REG write-lock.
      CPU0_LOCK_REG_LOCK : CPU0_LOCK_REG_CPU0_LOCK_REG_LOCK_Field :=
                            NXP_SVD.AHB_SECURE_CTRL.Writable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPU0_LOCK_REG_Register use record
      LOCK_NS_VTOR       at 0 range 0 .. 1;
      LOCK_NS_MPU        at 0 range 2 .. 3;
      LOCK_S_VTAIRCR     at 0 range 4 .. 5;
      LOCK_S_MPU         at 0 range 6 .. 7;
      LOCK_SAU           at 0 range 8 .. 9;
      Reserved_10_29     at 0 range 10 .. 29;
      CPU0_LOCK_REG_LOCK at 0 range 30 .. 31;
   end record;

   --  micro-Cortex M33 (CPU1) VTOR_NS register write-lock.
   type CPU1_LOCK_REG_LOCK_NS_VTOR_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for CPU1_LOCK_REG_LOCK_NS_VTOR_Field use
     (Blocked => 1,
      Writable => 2);

   --  micro-Cortex M33 (CPU1) non-secure MPU register write-lock.
   type CPU1_LOCK_REG_LOCK_NS_MPU_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for CPU1_LOCK_REG_LOCK_NS_MPU_Field use
     (Blocked => 1,
      Writable => 2);

   --  CPU1_LOCK_REG write-lock.
   type CPU1_LOCK_REG_CPU1_LOCK_REG_LOCK_Field is
     (
      --  Restricted mode.
      Blocked,
      --  Writable.
      Writable)
     with Size => 2;
   for CPU1_LOCK_REG_CPU1_LOCK_REG_LOCK_Field use
     (Blocked => 1,
      Writable => 2);

   --  Miscalleneous control signals for in micro-Cortex M33 (CPU1)
   type CPU1_LOCK_REG_Register is record
      --  micro-Cortex M33 (CPU1) VTOR_NS register write-lock.
      LOCK_NS_VTOR       : CPU1_LOCK_REG_LOCK_NS_VTOR_Field :=
                            NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  micro-Cortex M33 (CPU1) non-secure MPU register write-lock.
      LOCK_NS_MPU        : CPU1_LOCK_REG_LOCK_NS_MPU_Field :=
                            NXP_SVD.AHB_SECURE_CTRL.Writable;
      --  unspecified
      Reserved_4_29      : HAL.UInt26 := 16#0#;
      --  CPU1_LOCK_REG write-lock.
      CPU1_LOCK_REG_LOCK : CPU1_LOCK_REG_CPU1_LOCK_REG_LOCK_Field :=
                            NXP_SVD.AHB_SECURE_CTRL.Writable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPU1_LOCK_REG_Register use record
      LOCK_NS_VTOR       at 0 range 0 .. 1;
      LOCK_NS_MPU        at 0 range 2 .. 3;
      Reserved_4_29      at 0 range 4 .. 29;
      CPU1_LOCK_REG_LOCK at 0 range 30 .. 31;
   end record;

   --  Write lock.
   type MISC_CTRL_DP_REG_WRITE_LOCK_Field is
     (
      --  Restricted mode.
      Restricted,
      --  Secure control registers can be written.
      Accessible)
     with Size => 2;
   for MISC_CTRL_DP_REG_WRITE_LOCK_Field use
     (Restricted => 1,
      Accessible => 2);

   --  Enable secure check for AHB matrix.
   type MISC_CTRL_DP_REG_ENABLE_SECURE_CHECKING_Field is
     (
      --  Restricted mode.
      Enable,
      --  Disable check.
      Disable)
     with Size => 2;
   for MISC_CTRL_DP_REG_ENABLE_SECURE_CHECKING_Field use
     (Enable => 1,
      Disable => 2);

   --  Enable secure privilege check for AHB matrix.
   type MISC_CTRL_DP_REG_ENABLE_S_PRIV_CHECK_Field is
     (
      --  Restricted mode.
      Enable,
      --  Disable check.
      Disable)
     with Size => 2;
   for MISC_CTRL_DP_REG_ENABLE_S_PRIV_CHECK_Field use
     (Enable => 1,
      Disable => 2);

   --  Enable non-secure privilege check for AHB matrix.
   type MISC_CTRL_DP_REG_ENABLE_NS_PRIV_CHECK_Field is
     (
      --  Restricted mode.
      Enable,
      --  Disable check.
      Disable)
     with Size => 2;
   for MISC_CTRL_DP_REG_ENABLE_NS_PRIV_CHECK_Field use
     (Enable => 1,
      Disable => 2);

   --  Disable secure violation abort.
   type MISC_CTRL_DP_REG_DISABLE_VIOLATION_ABORT_Field is
     (
      --  Disable abort fort secure checker.
      Disable,
      --  Enable abort fort secure checker.
      Enable)
     with Size => 2;
   for MISC_CTRL_DP_REG_DISABLE_VIOLATION_ABORT_Field use
     (Disable => 1,
      Enable => 2);

   --  Disable simple master strict mode.
   type MISC_CTRL_DP_REG_DISABLE_SIMPLE_MASTER_STRICT_MODE_Field is
     (
      --  Simple master in tier mode.
      Tier_Mode,
      --  Simple master in strict mode.
      Strict_Mode)
     with Size => 2;
   for MISC_CTRL_DP_REG_DISABLE_SIMPLE_MASTER_STRICT_MODE_Field use
     (Tier_Mode => 1,
      Strict_Mode => 2);

   --  Disable smart master strict mode.
   type MISC_CTRL_DP_REG_DISABLE_SMART_MASTER_STRICT_MODE_Field is
     (
      --  Smart master in tier mode.
      Tier_Mode,
      --  Smart master in strict mode.
      Strict_Mode)
     with Size => 2;
   for MISC_CTRL_DP_REG_DISABLE_SMART_MASTER_STRICT_MODE_Field use
     (Tier_Mode => 1,
      Strict_Mode => 2);

   --  Disable IDAU.
   type MISC_CTRL_DP_REG_IDAU_ALL_NS_Field is
     (
      --  IDAU is disable.
      Disable,
      --  IDAU is enabled.
      Enable)
     with Size => 2;
   for MISC_CTRL_DP_REG_IDAU_ALL_NS_Field use
     (Disable => 1,
      Enable => 2);

   --  secure control duplicate register
   type MISC_CTRL_DP_REG_Register is record
      --  Write lock.
      WRITE_LOCK                        : MISC_CTRL_DP_REG_WRITE_LOCK_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Accessible;
      --  Enable secure check for AHB matrix.
      ENABLE_SECURE_CHECKING            : MISC_CTRL_DP_REG_ENABLE_SECURE_CHECKING_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Disable;
      --  Enable secure privilege check for AHB matrix.
      ENABLE_S_PRIV_CHECK               : MISC_CTRL_DP_REG_ENABLE_S_PRIV_CHECK_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Disable;
      --  Enable non-secure privilege check for AHB matrix.
      ENABLE_NS_PRIV_CHECK              : MISC_CTRL_DP_REG_ENABLE_NS_PRIV_CHECK_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Disable;
      --  Disable secure violation abort.
      DISABLE_VIOLATION_ABORT           : MISC_CTRL_DP_REG_DISABLE_VIOLATION_ABORT_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Enable;
      --  Disable simple master strict mode.
      DISABLE_SIMPLE_MASTER_STRICT_MODE : MISC_CTRL_DP_REG_DISABLE_SIMPLE_MASTER_STRICT_MODE_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Strict_Mode;
      --  Disable smart master strict mode.
      DISABLE_SMART_MASTER_STRICT_MODE  : MISC_CTRL_DP_REG_DISABLE_SMART_MASTER_STRICT_MODE_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Strict_Mode;
      --  Disable IDAU.
      IDAU_ALL_NS                       : MISC_CTRL_DP_REG_IDAU_ALL_NS_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Enable;
      --  unspecified
      Reserved_16_31                    : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISC_CTRL_DP_REG_Register use record
      WRITE_LOCK                        at 0 range 0 .. 1;
      ENABLE_SECURE_CHECKING            at 0 range 2 .. 3;
      ENABLE_S_PRIV_CHECK               at 0 range 4 .. 5;
      ENABLE_NS_PRIV_CHECK              at 0 range 6 .. 7;
      DISABLE_VIOLATION_ABORT           at 0 range 8 .. 9;
      DISABLE_SIMPLE_MASTER_STRICT_MODE at 0 range 10 .. 11;
      DISABLE_SMART_MASTER_STRICT_MODE  at 0 range 12 .. 13;
      IDAU_ALL_NS                       at 0 range 14 .. 15;
      Reserved_16_31                    at 0 range 16 .. 31;
   end record;

   --  Write lock.
   type MISC_CTRL_REG_WRITE_LOCK_Field is
     (
      --  Restricted mode.
      Restricted,
      --  Secure control registers can be written.
      Accessible)
     with Size => 2;
   for MISC_CTRL_REG_WRITE_LOCK_Field use
     (Restricted => 1,
      Accessible => 2);

   --  Enable secure check for AHB matrix.
   type MISC_CTRL_REG_ENABLE_SECURE_CHECKING_Field is
     (
      --  Restricted mode.
      Enable,
      --  Disable check.
      Disable)
     with Size => 2;
   for MISC_CTRL_REG_ENABLE_SECURE_CHECKING_Field use
     (Enable => 1,
      Disable => 2);

   --  Enable secure privilege check for AHB matrix.
   type MISC_CTRL_REG_ENABLE_S_PRIV_CHECK_Field is
     (
      --  Restricted mode.
      Enable,
      --  Disable check.
      Disable)
     with Size => 2;
   for MISC_CTRL_REG_ENABLE_S_PRIV_CHECK_Field use
     (Enable => 1,
      Disable => 2);

   --  Enable non-secure privilege check for AHB matrix.
   type MISC_CTRL_REG_ENABLE_NS_PRIV_CHECK_Field is
     (
      --  Restricted mode.
      Enable,
      --  Disable check.
      Disable)
     with Size => 2;
   for MISC_CTRL_REG_ENABLE_NS_PRIV_CHECK_Field use
     (Enable => 1,
      Disable => 2);

   --  Disable secure violation abort.
   type MISC_CTRL_REG_DISABLE_VIOLATION_ABORT_Field is
     (
      --  Disable abort fort secure checker.
      Disable,
      --  Enable abort fort secure checker.
      Enable)
     with Size => 2;
   for MISC_CTRL_REG_DISABLE_VIOLATION_ABORT_Field use
     (Disable => 1,
      Enable => 2);

   --  Disable simple master strict mode.
   type MISC_CTRL_REG_DISABLE_SIMPLE_MASTER_STRICT_MODE_Field is
     (
      --  Simple master in tier mode.
      Tier_Mode,
      --  Simple master in strict mode.
      Strict_Mode)
     with Size => 2;
   for MISC_CTRL_REG_DISABLE_SIMPLE_MASTER_STRICT_MODE_Field use
     (Tier_Mode => 1,
      Strict_Mode => 2);

   --  Disable smart master strict mode.
   type MISC_CTRL_REG_DISABLE_SMART_MASTER_STRICT_MODE_Field is
     (
      --  Smart master in tier mode.
      Tier_Mode,
      --  Smart master in strict mode.
      Strict_Mode)
     with Size => 2;
   for MISC_CTRL_REG_DISABLE_SMART_MASTER_STRICT_MODE_Field use
     (Tier_Mode => 1,
      Strict_Mode => 2);

   --  Disable IDAU.
   type MISC_CTRL_REG_IDAU_ALL_NS_Field is
     (
      --  IDAU is disable.
      Disable,
      --  IDAU is enabled.
      Enable)
     with Size => 2;
   for MISC_CTRL_REG_IDAU_ALL_NS_Field use
     (Disable => 1,
      Enable => 2);

   --  secure control register
   type MISC_CTRL_REG_Register is record
      --  Write lock.
      WRITE_LOCK                        : MISC_CTRL_REG_WRITE_LOCK_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Accessible;
      --  Enable secure check for AHB matrix.
      ENABLE_SECURE_CHECKING            : MISC_CTRL_REG_ENABLE_SECURE_CHECKING_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Disable;
      --  Enable secure privilege check for AHB matrix.
      ENABLE_S_PRIV_CHECK               : MISC_CTRL_REG_ENABLE_S_PRIV_CHECK_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Disable;
      --  Enable non-secure privilege check for AHB matrix.
      ENABLE_NS_PRIV_CHECK              : MISC_CTRL_REG_ENABLE_NS_PRIV_CHECK_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Disable;
      --  Disable secure violation abort.
      DISABLE_VIOLATION_ABORT           : MISC_CTRL_REG_DISABLE_VIOLATION_ABORT_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Enable;
      --  Disable simple master strict mode.
      DISABLE_SIMPLE_MASTER_STRICT_MODE : MISC_CTRL_REG_DISABLE_SIMPLE_MASTER_STRICT_MODE_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Strict_Mode;
      --  Disable smart master strict mode.
      DISABLE_SMART_MASTER_STRICT_MODE  : MISC_CTRL_REG_DISABLE_SMART_MASTER_STRICT_MODE_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Strict_Mode;
      --  Disable IDAU.
      IDAU_ALL_NS                       : MISC_CTRL_REG_IDAU_ALL_NS_Field :=
                                           NXP_SVD.AHB_SECURE_CTRL.Enable;
      --  unspecified
      Reserved_16_31                    : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISC_CTRL_REG_Register use record
      WRITE_LOCK                        at 0 range 0 .. 1;
      ENABLE_SECURE_CHECKING            at 0 range 2 .. 3;
      ENABLE_S_PRIV_CHECK               at 0 range 4 .. 5;
      ENABLE_NS_PRIV_CHECK              at 0 range 6 .. 7;
      DISABLE_VIOLATION_ABORT           at 0 range 8 .. 9;
      DISABLE_SIMPLE_MASTER_STRICT_MODE at 0 range 10 .. 11;
      DISABLE_SMART_MASTER_STRICT_MODE  at 0 range 12 .. 13;
      IDAU_ALL_NS                       at 0 range 14 .. 15;
      Reserved_16_31                    at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  AHB secure controller
   type AHB_SECURE_CTRL_Peripheral is record
      --  Security access rules for Flash and ROM slaves.
      SEC_CTRL_FLASH_ROM_SLAVE_RULE   : aliased SEC_CTRL_FLASH_ROM_SLAVE_RULE_Register;
      --  Security access rules for FLASH sector 0 to sector 20. Each Flash
      --  sector is 32 Kbytes. There are 20 FLASH sectors in total.
      SEC_CTRL_FLASH_MEM_RULE0        : aliased SEC_CTRL_FLASH_MEM_RULE_Register;
      --  Security access rules for FLASH sector 0 to sector 20. Each Flash
      --  sector is 32 Kbytes. There are 20 FLASH sectors in total.
      SEC_CTRL_FLASH_MEM_RULE1        : aliased SEC_CTRL_FLASH_MEM_RULE_Register;
      --  Security access rules for FLASH sector 0 to sector 20. Each Flash
      --  sector is 32 Kbytes. There are 20 FLASH sectors in total.
      SEC_CTRL_FLASH_MEM_RULE2        : aliased SEC_CTRL_FLASH_MEM_RULE_Register;
      --  Security access rules for ROM sector 0 to sector 31. Each ROM sector
      --  is 4 Kbytes. There are 32 ROM sectors in total.
      SEC_CTRL_ROM_MEM_RULE0          : aliased SEC_CTRL_ROM_MEM_RULE_Register;
      --  Security access rules for ROM sector 0 to sector 31. Each ROM sector
      --  is 4 Kbytes. There are 32 ROM sectors in total.
      SEC_CTRL_ROM_MEM_RULE1          : aliased SEC_CTRL_ROM_MEM_RULE_Register;
      --  Security access rules for ROM sector 0 to sector 31. Each ROM sector
      --  is 4 Kbytes. There are 32 ROM sectors in total.
      SEC_CTRL_ROM_MEM_RULE2          : aliased SEC_CTRL_ROM_MEM_RULE_Register;
      --  Security access rules for ROM sector 0 to sector 31. Each ROM sector
      --  is 4 Kbytes. There are 32 ROM sectors in total.
      SEC_CTRL_ROM_MEM_RULE3          : aliased SEC_CTRL_ROM_MEM_RULE_Register;
      --  Security access rules for RAMX slaves.
      SEC_CTRL_RAMX_SLAVE_RULE        : aliased SEC_CTRL_RAMX_SLAVE_RULE_Register;
      --  Security access rules for RAMX slaves.
      SEC_CTRL_RAMX_MEM_RULE0         : aliased SEC_CTRL_RAMX_MEM_RULE0_Register;
      --  Security access rules for RAM0 slaves.
      SEC_CTRL_RAM0_SLAVE_RULE        : aliased SEC_CTRL_RAM0_SLAVE_RULE_Register;
      --  Security access rules for RAM0 slaves.
      SEC_CTRL_RAM0_MEM_RULE0         : aliased SEC_CTRL_RAM0_MEM_RULE_Register;
      --  Security access rules for RAM0 slaves.
      SEC_CTRL_RAM0_MEM_RULE1         : aliased SEC_CTRL_RAM0_MEM_RULE_Register;
      --  Security access rules for RAM1 slaves.
      SEC_CTRL_RAM1_SLAVE_RULE        : aliased SEC_CTRL_RAM1_SLAVE_RULE_Register;
      --  Security access rules for RAM1 slaves.
      SEC_CTRL_RAM1_MEM_RULE0         : aliased SEC_CTRL_RAM1_MEM_RULE_Register;
      --  Security access rules for RAM1 slaves.
      SEC_CTRL_RAM1_MEM_RULE1         : aliased SEC_CTRL_RAM1_MEM_RULE_Register;
      --  Security access rules for RAM2 slaves.
      SEC_CTRL_RAM2_SLAVE_RULE        : aliased SEC_CTRL_RAM2_SLAVE_RULE_Register;
      --  Security access rules for RAM2 slaves.
      SEC_CTRL_RAM2_MEM_RULE0         : aliased SEC_CTRL_RAM2_MEM_RULE_Register;
      --  Security access rules for RAM2 slaves.
      SEC_CTRL_RAM2_MEM_RULE1         : aliased SEC_CTRL_RAM2_MEM_RULE_Register;
      --  Security access rules for RAM3 slaves.
      SEC_CTRL_RAM3_SLAVE_RULE        : aliased SEC_CTRL_RAM3_SLAVE_RULE_Register;
      --  Security access rules for RAM3 slaves.
      SEC_CTRL_RAM3_MEM_RULE0         : aliased SEC_CTRL_RAM3_MEM_RULE_Register;
      --  Security access rules for RAM3 slaves.
      SEC_CTRL_RAM3_MEM_RULE1         : aliased SEC_CTRL_RAM3_MEM_RULE_Register;
      --  Security access rules for RAM4 slaves.
      SEC_CTRL_RAM4_SLAVE_RULE        : aliased SEC_CTRL_RAM4_SLAVE_RULE_Register;
      --  Security access rules for RAM4 slaves.
      SEC_CTRL_RAM4_MEM_RULE0         : aliased SEC_CTRL_RAM4_MEM_RULE0_Register;
      --  Security access rules for both APB Bridges slaves.
      SEC_CTRL_APB_BRIDGE_SLAVE_RULE  : aliased SEC_CTRL_APB_BRIDGE_SLAVE_RULE_Register;
      --  Security access rules for APB Bridge 0 peripherals. Each APB bridge
      --  sector is 4 Kbytes. There are 32 APB Bridge 0 sectors in total.
      SEC_CTRL_APB_BRIDGE0_MEM_CTRL0  : aliased SEC_CTRL_APB_BRIDGE0_MEM_CTRL0_Register;
      --  Security access rules for APB Bridge 0 peripherals. Each APB bridge
      --  sector is 4 Kbytes. There are 32 APB Bridge 0 sectors in total.
      SEC_CTRL_APB_BRIDGE0_MEM_CTRL1  : aliased SEC_CTRL_APB_BRIDGE0_MEM_CTRL1_Register;
      --  Security access rules for APB Bridge 0 peripherals. Each APB bridge
      --  sector is 4 Kbytes. There are 32 APB Bridge 0 sectors in total.
      SEC_CTRL_APB_BRIDGE0_MEM_CTRL2  : aliased SEC_CTRL_APB_BRIDGE0_MEM_CTRL2_Register;
      --  Security access rules for APB Bridge 1 peripherals. Each APB bridge
      --  sector is 4 Kbytes. There are 32 APB Bridge 1 sectors in total.
      SEC_CTRL_APB_BRIDGE1_MEM_CTRL0  : aliased SEC_CTRL_APB_BRIDGE1_MEM_CTRL0_Register;
      --  Security access rules for APB Bridge 1 peripherals. Each APB bridge
      --  sector is 4 Kbytes. There are 32 APB Bridge 1 sectors in total.
      SEC_CTRL_APB_BRIDGE1_MEM_CTRL1  : aliased SEC_CTRL_APB_BRIDGE1_MEM_CTRL1_Register;
      --  Security access rules for APB Bridge 1 peripherals. Each APB bridge
      --  sector is 4 Kbytes. There are 32 APB Bridge 1 sectors in total.
      SEC_CTRL_APB_BRIDGE1_MEM_CTRL2  : aliased SEC_CTRL_APB_BRIDGE1_MEM_CTRL2_Register;
      --  Security access rules for APB Bridge 1 peripherals. Each APB bridge
      --  sector is 4 Kbytes. There are 32 APB Bridge 1 sectors in total.
      SEC_CTRL_APB_BRIDGE1_MEM_CTRL3  : aliased SEC_CTRL_APB_BRIDGE1_MEM_CTRL3_Register;
      --  Security access rules for AHB peripherals.
      SEC_CTRL_AHB_PORT8_SLAVE0_RULE  : aliased SEC_CTRL_AHB_PORT8_SLAVE0_RULE_Register;
      --  Security access rules for AHB peripherals.
      SEC_CTRL_AHB_PORT8_SLAVE1_RULE  : aliased SEC_CTRL_AHB_PORT8_SLAVE1_RULE_Register;
      --  Security access rules for AHB peripherals.
      SEC_CTRL_AHB_PORT9_SLAVE0_RULE  : aliased SEC_CTRL_AHB_PORT9_SLAVE0_RULE_Register;
      --  Security access rules for AHB peripherals.
      SEC_CTRL_AHB_PORT9_SLAVE1_RULE  : aliased SEC_CTRL_AHB_PORT9_SLAVE1_RULE_Register;
      --  Security access rules for AHB peripherals.
      SEC_CTRL_AHB_PORT10_SLAVE0_RULE : aliased SEC_CTRL_AHB_PORT10_SLAVE0_RULE_Register;
      --  Security access rules for AHB peripherals.
      SEC_CTRL_AHB_PORT10_SLAVE1_RULE : aliased SEC_CTRL_AHB_PORT10_SLAVE1_RULE_Register;
      --  Security access rules for AHB_SEC_CTRL_AHB.
      SEC_CTRL_AHB_SEC_CTRL_MEM_RULE  : aliased SEC_CTRL_AHB_SEC_CTRL_MEM_RULE_Register;
      --  Security access rules for USB High speed RAM slaves.
      SEC_CTRL_USB_HS_SLAVE_RULE      : aliased SEC_CTRL_USB_HS_SLAVE_RULE_Register;
      --  Security access rules for RAM_USB_HS.
      SEC_CTRL_USB_HS_MEM_RULE        : aliased SEC_CTRL_USB_HS_MEM_RULE_Register;
      --  most recent security violation address for AHB port n
      sec_vio_addr                    : aliased sec_vio_addr_Registers;
      --  most recent security violation miscellaneous information for AHB port
      --  n
      sec_vio_misc_info               : aliased sec_vio_misc_info_Registers;
      --  security violation address/information registers valid flags
      SEC_VIO_INFO_VALID              : aliased SEC_VIO_INFO_VALID_Register;
      --  Secure GPIO mask for port 0 pins.
      SEC_GPIO_MASK0                  : aliased SEC_GPIO_MASK0_Register;
      --  Secure GPIO mask for port 1 pins.
      SEC_GPIO_MASK1                  : aliased SEC_GPIO_MASK1_Register;
      --  Secure Interrupt mask for CPU1
      SEC_CPU_INT_MASK0               : aliased SEC_CPU_INT_MASK0_Register;
      --  Secure Interrupt mask for CPU1
      SEC_CPU_INT_MASK1               : aliased SEC_CPU_INT_MASK1_Register;
      --  Security General Purpose register access control.
      SEC_MASK_LOCK                   : aliased SEC_MASK_LOCK_Register;
      --  master secure level register
      MASTER_SEC_LEVEL                : aliased MASTER_SEC_LEVEL_Register;
      --  master secure level anti-pole register
      MASTER_SEC_ANTI_POL_REG         : aliased MASTER_SEC_ANTI_POL_REG_Register;
      --  Miscalleneous control signals for in Cortex M33 (CPU0)
      CPU0_LOCK_REG                   : aliased CPU0_LOCK_REG_Register;
      --  Miscalleneous control signals for in micro-Cortex M33 (CPU1)
      CPU1_LOCK_REG                   : aliased CPU1_LOCK_REG_Register;
      --  secure control duplicate register
      MISC_CTRL_DP_REG                : aliased MISC_CTRL_DP_REG_Register;
      --  secure control register
      MISC_CTRL_REG                   : aliased MISC_CTRL_REG_Register;
   end record
     with Volatile;

   for AHB_SECURE_CTRL_Peripheral use record
      SEC_CTRL_FLASH_ROM_SLAVE_RULE   at 16#0# range 0 .. 31;
      SEC_CTRL_FLASH_MEM_RULE0        at 16#10# range 0 .. 31;
      SEC_CTRL_FLASH_MEM_RULE1        at 16#14# range 0 .. 31;
      SEC_CTRL_FLASH_MEM_RULE2        at 16#18# range 0 .. 31;
      SEC_CTRL_ROM_MEM_RULE0          at 16#20# range 0 .. 31;
      SEC_CTRL_ROM_MEM_RULE1          at 16#24# range 0 .. 31;
      SEC_CTRL_ROM_MEM_RULE2          at 16#28# range 0 .. 31;
      SEC_CTRL_ROM_MEM_RULE3          at 16#2C# range 0 .. 31;
      SEC_CTRL_RAMX_SLAVE_RULE        at 16#30# range 0 .. 31;
      SEC_CTRL_RAMX_MEM_RULE0         at 16#40# range 0 .. 31;
      SEC_CTRL_RAM0_SLAVE_RULE        at 16#50# range 0 .. 31;
      SEC_CTRL_RAM0_MEM_RULE0         at 16#60# range 0 .. 31;
      SEC_CTRL_RAM0_MEM_RULE1         at 16#64# range 0 .. 31;
      SEC_CTRL_RAM1_SLAVE_RULE        at 16#70# range 0 .. 31;
      SEC_CTRL_RAM1_MEM_RULE0         at 16#80# range 0 .. 31;
      SEC_CTRL_RAM1_MEM_RULE1         at 16#84# range 0 .. 31;
      SEC_CTRL_RAM2_SLAVE_RULE        at 16#90# range 0 .. 31;
      SEC_CTRL_RAM2_MEM_RULE0         at 16#A0# range 0 .. 31;
      SEC_CTRL_RAM2_MEM_RULE1         at 16#A4# range 0 .. 31;
      SEC_CTRL_RAM3_SLAVE_RULE        at 16#B0# range 0 .. 31;
      SEC_CTRL_RAM3_MEM_RULE0         at 16#C0# range 0 .. 31;
      SEC_CTRL_RAM3_MEM_RULE1         at 16#C4# range 0 .. 31;
      SEC_CTRL_RAM4_SLAVE_RULE        at 16#D0# range 0 .. 31;
      SEC_CTRL_RAM4_MEM_RULE0         at 16#E0# range 0 .. 31;
      SEC_CTRL_APB_BRIDGE_SLAVE_RULE  at 16#F0# range 0 .. 31;
      SEC_CTRL_APB_BRIDGE0_MEM_CTRL0  at 16#100# range 0 .. 31;
      SEC_CTRL_APB_BRIDGE0_MEM_CTRL1  at 16#104# range 0 .. 31;
      SEC_CTRL_APB_BRIDGE0_MEM_CTRL2  at 16#108# range 0 .. 31;
      SEC_CTRL_APB_BRIDGE1_MEM_CTRL0  at 16#110# range 0 .. 31;
      SEC_CTRL_APB_BRIDGE1_MEM_CTRL1  at 16#114# range 0 .. 31;
      SEC_CTRL_APB_BRIDGE1_MEM_CTRL2  at 16#118# range 0 .. 31;
      SEC_CTRL_APB_BRIDGE1_MEM_CTRL3  at 16#11C# range 0 .. 31;
      SEC_CTRL_AHB_PORT8_SLAVE0_RULE  at 16#120# range 0 .. 31;
      SEC_CTRL_AHB_PORT8_SLAVE1_RULE  at 16#124# range 0 .. 31;
      SEC_CTRL_AHB_PORT9_SLAVE0_RULE  at 16#130# range 0 .. 31;
      SEC_CTRL_AHB_PORT9_SLAVE1_RULE  at 16#134# range 0 .. 31;
      SEC_CTRL_AHB_PORT10_SLAVE0_RULE at 16#140# range 0 .. 31;
      SEC_CTRL_AHB_PORT10_SLAVE1_RULE at 16#144# range 0 .. 31;
      SEC_CTRL_AHB_SEC_CTRL_MEM_RULE  at 16#150# range 0 .. 31;
      SEC_CTRL_USB_HS_SLAVE_RULE      at 16#160# range 0 .. 31;
      SEC_CTRL_USB_HS_MEM_RULE        at 16#170# range 0 .. 31;
      sec_vio_addr                    at 16#E00# range 0 .. 383;
      sec_vio_misc_info               at 16#E80# range 0 .. 383;
      SEC_VIO_INFO_VALID              at 16#F00# range 0 .. 31;
      SEC_GPIO_MASK0                  at 16#F80# range 0 .. 31;
      SEC_GPIO_MASK1                  at 16#F84# range 0 .. 31;
      SEC_CPU_INT_MASK0               at 16#F90# range 0 .. 31;
      SEC_CPU_INT_MASK1               at 16#F94# range 0 .. 31;
      SEC_MASK_LOCK                   at 16#FBC# range 0 .. 31;
      MASTER_SEC_LEVEL                at 16#FD0# range 0 .. 31;
      MASTER_SEC_ANTI_POL_REG         at 16#FD4# range 0 .. 31;
      CPU0_LOCK_REG                   at 16#FEC# range 0 .. 31;
      CPU1_LOCK_REG                   at 16#FF0# range 0 .. 31;
      MISC_CTRL_DP_REG                at 16#FF8# range 0 .. 31;
      MISC_CTRL_REG                   at 16#FFC# range 0 .. 31;
   end record;

   --  AHB secure controller
   AHB_SECURE_CTRL_Periph : aliased AHB_SECURE_CTRL_Peripheral
     with Import, Address => System'To_Address (16#400AC000#);

end NXP_SVD.AHB_SECURE_CTRL;
