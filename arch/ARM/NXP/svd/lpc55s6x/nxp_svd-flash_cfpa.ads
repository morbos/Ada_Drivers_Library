--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.FLASH_CFPA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype ROTKH_REVOKE_RoTK0_EN_Field is HAL.UInt2;
   subtype ROTKH_REVOKE_RoTK1_EN_Field is HAL.UInt2;
   subtype ROTKH_REVOKE_RoTK2_EN_Field is HAL.UInt2;

   --  .
   type ROTKH_REVOKE_Register is record
      --  RoT Key 0 enable. 00 - Invalid 01 - Enabled 10, 11 - Key revoked
      RoTK0_EN      : ROTKH_REVOKE_RoTK0_EN_Field := 16#0#;
      --  RoT Key 1 enable. 00 - Invalid 01 - Enabled 10, 11 - Key revoked
      RoTK1_EN      : ROTKH_REVOKE_RoTK1_EN_Field := 16#0#;
      --  RoT Key 2 enable. 00 - Invalid 01 - Enabled 10, 11 - Key revoked
      RoTK2_EN      : ROTKH_REVOKE_RoTK2_EN_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ROTKH_REVOKE_Register use record
      RoTK0_EN      at 0 range 0 .. 1;
      RoTK1_EN      at 0 range 2 .. 3;
      RoTK2_EN      at 0 range 4 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype VENDOR_USAGE_DBG_VENDOR_USAGE_Field is HAL.UInt16;
   subtype VENDOR_USAGE_INVERSE_VALUE_Field is HAL.UInt16;

   --  .
   type VENDOR_USAGE_Register is record
      --  DBG_VENDOR_USAGE.
      DBG_VENDOR_USAGE : VENDOR_USAGE_DBG_VENDOR_USAGE_Field := 16#0#;
      --  inverse value of bits [15:0]
      INVERSE_VALUE    : VENDOR_USAGE_INVERSE_VALUE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for VENDOR_USAGE_Register use record
      DBG_VENDOR_USAGE at 0 range 0 .. 15;
      INVERSE_VALUE    at 0 range 16 .. 31;
   end record;

   --  Non Secure non-invasive debug enable
   type DCFG_CC_SOCU_PIN_NIDEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_NIDEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Non Secure debug enable
   type DCFG_CC_SOCU_PIN_DBGEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_DBGEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Secure non-invasive debug enable
   type DCFG_CC_SOCU_PIN_SPNIDEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_SPNIDEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Secure invasive debug enable
   type DCFG_CC_SOCU_PIN_SPIDEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_SPIDEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  JTAG TAP enable
   type DCFG_CC_SOCU_PIN_TAPEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_TAPEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  CPU1 (Micro cortex M33) invasive debug enable
   type DCFG_CC_SOCU_PIN_CPU1_DBGEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_CPU1_DBGEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  ISP Boot Command enable
   type DCFG_CC_SOCU_PIN_ISP_CMD_EN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_ISP_CMD_EN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  FA Command enable
   type DCFG_CC_SOCU_PIN_FA_CMD_EN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_FA_CMD_EN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Flash Mass Erase Command enable
   type DCFG_CC_SOCU_PIN_ME_CMD_EN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_ME_CMD_EN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  CPU1 (Micro cortex M33) non-invasive debug enable
   type DCFG_CC_SOCU_PIN_CPU1_NIDEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for DCFG_CC_SOCU_PIN_CPU1_NIDEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   subtype DCFG_CC_SOCU_PIN_INVERSE_VALUE_Field is HAL.UInt16;

   --  With TZ-M, the part can be sold by level 1 customers (secure code
   --  developer) to level-2 customers who develops non-secure code only. - In
   --  this scenario, or easy of development, Level-I customer releases the
   --  part to always allow non-secure debug. - To allow level-2 customers to
   --  further seal the part DCFG_CC_SOCU_NS is used. - ROM will use this word
   --  to further restrict the debug access.
   type DCFG_CC_SOCU_PIN_Register is record
      --  Non Secure non-invasive debug enable
      NIDEN          : DCFG_CC_SOCU_PIN_NIDEN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  Non Secure debug enable
      DBGEN          : DCFG_CC_SOCU_PIN_DBGEN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  Secure non-invasive debug enable
      SPNIDEN        : DCFG_CC_SOCU_PIN_SPNIDEN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  Secure invasive debug enable
      SPIDEN         : DCFG_CC_SOCU_PIN_SPIDEN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  JTAG TAP enable
      TAPEN          : DCFG_CC_SOCU_PIN_TAPEN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  CPU1 (Micro cortex M33) invasive debug enable
      CPU1_DBGEN     : DCFG_CC_SOCU_PIN_CPU1_DBGEN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  ISP Boot Command enable
      ISP_CMD_EN     : DCFG_CC_SOCU_PIN_ISP_CMD_EN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  FA Command enable
      FA_CMD_EN      : DCFG_CC_SOCU_PIN_FA_CMD_EN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  Flash Mass Erase Command enable
      ME_CMD_EN      : DCFG_CC_SOCU_PIN_ME_CMD_EN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  CPU1 (Micro cortex M33) non-invasive debug enable
      CPU1_NIDEN     : DCFG_CC_SOCU_PIN_CPU1_NIDEN_Field :=
                        NXP_SVD.FLASH_CFPA.Value_0;
      --  unspecified
      Reserved_10_14 : HAL.UInt5 := 16#0#;
      --  Enforce UUID match during Debug authentication.
      UUID_CHECK     : Boolean := False;
      --  inverse value of bits [15:0]
      INVERSE_VALUE  : DCFG_CC_SOCU_PIN_INVERSE_VALUE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCFG_CC_SOCU_PIN_Register use record
      NIDEN          at 0 range 0 .. 0;
      DBGEN          at 0 range 1 .. 1;
      SPNIDEN        at 0 range 2 .. 2;
      SPIDEN         at 0 range 3 .. 3;
      TAPEN          at 0 range 4 .. 4;
      CPU1_DBGEN     at 0 range 5 .. 5;
      ISP_CMD_EN     at 0 range 6 .. 6;
      FA_CMD_EN      at 0 range 7 .. 7;
      ME_CMD_EN      at 0 range 8 .. 8;
      CPU1_NIDEN     at 0 range 9 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      UUID_CHECK     at 0 range 15 .. 15;
      INVERSE_VALUE  at 0 range 16 .. 31;
   end record;

   --  Non Secure non-invasive debug fixed state
   type DCFG_CC_SOCU_DFLT_NIDEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_NIDEN_Field use
     (Disable => 0,
      Enable => 1);

   --  Non Secure debug fixed state
   type DCFG_CC_SOCU_DFLT_DBGEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_DBGEN_Field use
     (Disable => 0,
      Enable => 1);

   --  Secure non-invasive debug fixed state
   type DCFG_CC_SOCU_DFLT_SPNIDEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_SPNIDEN_Field use
     (Disable => 0,
      Enable => 1);

   --  Secure invasive debug fixed state
   type DCFG_CC_SOCU_DFLT_SPIDEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_SPIDEN_Field use
     (Disable => 0,
      Enable => 1);

   --  JTAG TAP fixed state
   type DCFG_CC_SOCU_DFLT_TAPEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_TAPEN_Field use
     (Disable => 0,
      Enable => 1);

   --  CPU1 (Micro cortex M33) invasive debug fixed state
   type DCFG_CC_SOCU_DFLT_CPU1_DBGEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_CPU1_DBGEN_Field use
     (Disable => 0,
      Enable => 1);

   --  ISP Boot Command fixed state
   type DCFG_CC_SOCU_DFLT_ISP_CMD_EN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_ISP_CMD_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  FA Command fixed state
   type DCFG_CC_SOCU_DFLT_FA_CMD_EN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_FA_CMD_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  Flash Mass Erase Command fixed state
   type DCFG_CC_SOCU_DFLT_ME_CMD_EN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_ME_CMD_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  CPU1 (Micro cortex M33) non-invasive debug fixed state
   type DCFG_CC_SOCU_DFLT_CPU1_NIDEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for DCFG_CC_SOCU_DFLT_CPU1_NIDEN_Field use
     (Disable => 0,
      Enable => 1);

   subtype DCFG_CC_SOCU_DFLT_INVERSE_VALUE_Field is HAL.UInt16;

   --  With TZ-M, the part can be sold by level 1 customers (secure code
   --  developer) to level-2 customers who develops non-secure code only. - In
   --  this scenario, or easy of development, Level-I customer releases the
   --  part to always allow non-secure debug. - To allow level-2 customers to
   --  further seal the part DCFG_CC_SOCU_NS is used. - ROM will use this word
   --  to further restrict the debug access.
   type DCFG_CC_SOCU_DFLT_Register is record
      --  Non Secure non-invasive debug fixed state
      NIDEN          : DCFG_CC_SOCU_DFLT_NIDEN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  Non Secure debug fixed state
      DBGEN          : DCFG_CC_SOCU_DFLT_DBGEN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  Secure non-invasive debug fixed state
      SPNIDEN        : DCFG_CC_SOCU_DFLT_SPNIDEN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  Secure invasive debug fixed state
      SPIDEN         : DCFG_CC_SOCU_DFLT_SPIDEN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  JTAG TAP fixed state
      TAPEN          : DCFG_CC_SOCU_DFLT_TAPEN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  CPU1 (Micro cortex M33) invasive debug fixed state
      CPU1_DBGEN     : DCFG_CC_SOCU_DFLT_CPU1_DBGEN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  ISP Boot Command fixed state
      ISP_CMD_EN     : DCFG_CC_SOCU_DFLT_ISP_CMD_EN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  FA Command fixed state
      FA_CMD_EN      : DCFG_CC_SOCU_DFLT_FA_CMD_EN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  Flash Mass Erase Command fixed state
      ME_CMD_EN      : DCFG_CC_SOCU_DFLT_ME_CMD_EN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  CPU1 (Micro cortex M33) non-invasive debug fixed state
      CPU1_NIDEN     : DCFG_CC_SOCU_DFLT_CPU1_NIDEN_Field :=
                        NXP_SVD.FLASH_CFPA.Disable;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  inverse value of bits [15:0]
      INVERSE_VALUE  : DCFG_CC_SOCU_DFLT_INVERSE_VALUE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCFG_CC_SOCU_DFLT_Register use record
      NIDEN          at 0 range 0 .. 0;
      DBGEN          at 0 range 1 .. 1;
      SPNIDEN        at 0 range 2 .. 2;
      SPIDEN         at 0 range 3 .. 3;
      TAPEN          at 0 range 4 .. 4;
      CPU1_DBGEN     at 0 range 5 .. 5;
      ISP_CMD_EN     at 0 range 6 .. 6;
      FA_CMD_EN      at 0 range 7 .. 7;
      ME_CMD_EN      at 0 range 8 .. 8;
      CPU1_NIDEN     at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      INVERSE_VALUE  at 0 range 16 .. 31;
   end record;

   subtype PRINCE_REGION0_IV_HEADER1_TYPE_Field is HAL.UInt2;
   subtype PRINCE_REGION0_IV_HEADER1_INDEX_Field is HAL.UInt4;
   subtype PRINCE_REGION0_IV_HEADER1_SIZE_Field is HAL.UInt6;

   --  .
   type PRINCE_REGION0_IV_HEADER1_Register is record
      --  .
      TYPE_k         : PRINCE_REGION0_IV_HEADER1_TYPE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  .
      INDEX          : PRINCE_REGION0_IV_HEADER1_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  .
      SIZE           : PRINCE_REGION0_IV_HEADER1_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRINCE_REGION0_IV_HEADER1_Register use record
      TYPE_k         at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      INDEX          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      SIZE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype PRINCE_REGION1_IV_HEADER1_TYPE_Field is HAL.UInt2;
   subtype PRINCE_REGION1_IV_HEADER1_INDEX_Field is HAL.UInt4;
   subtype PRINCE_REGION1_IV_HEADER1_SIZE_Field is HAL.UInt6;

   --  .
   type PRINCE_REGION1_IV_HEADER1_Register is record
      --  .
      TYPE_k         : PRINCE_REGION1_IV_HEADER1_TYPE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  .
      INDEX          : PRINCE_REGION1_IV_HEADER1_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  .
      SIZE           : PRINCE_REGION1_IV_HEADER1_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRINCE_REGION1_IV_HEADER1_Register use record
      TYPE_k         at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      INDEX          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      SIZE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype PRINCE_REGION2_IV_HEADER1_TYPE_Field is HAL.UInt2;
   subtype PRINCE_REGION2_IV_HEADER1_INDEX_Field is HAL.UInt4;
   subtype PRINCE_REGION2_IV_HEADER1_SIZE_Field is HAL.UInt6;

   --  .
   type PRINCE_REGION2_IV_HEADER1_Register is record
      --  .
      TYPE_k         : PRINCE_REGION2_IV_HEADER1_TYPE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  .
      INDEX          : PRINCE_REGION2_IV_HEADER1_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  .
      SIZE           : PRINCE_REGION2_IV_HEADER1_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRINCE_REGION2_IV_HEADER1_Register use record
      TYPE_k         at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      INDEX          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      SIZE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Customer Defined (Programable through ROM API)

   --  Customer Defined (Programable through ROM API)
   type CUSTOMER_DEFINED_Registers is array (0 .. 55) of HAL.UInt32
     with Volatile;

   --  SHA256_DIGEST0 for DIGEST[31:0] SHA256_DIGEST1 for DIGEST[63:32] SHA256_DIGEST2 for DIGEST[95:64] SHA256_DIGEST3 for DIGEST[127:96] SHA256_DIGEST4 for DIGEST[159:128] SHA256_DIGEST5 for DIGEST[191:160] SHA256_DIGEST6 for DIGEST[223:192] SHA256_DIGEST7 for DIGEST[255:224]

   --  SHA256_DIGEST0 for DIGEST[31:0] SHA256_DIGEST1 for DIGEST[63:32]
   --  SHA256_DIGEST2 for DIGEST[95:64] SHA256_DIGEST3 for DIGEST[127:96]
   --  SHA256_DIGEST4 for DIGEST[159:128] SHA256_DIGEST5 for DIGEST[191:160]
   --  SHA256_DIGEST6 for DIGEST[223:192] SHA256_DIGEST7 for DIGEST[255:224]
   type SHA256_DIGEST_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   type FLASH_CFPA_Disc is
     (
      Code0,
      Header0,
      Code1,
      Header1,
      Body0,
      Code2,
      Body1,
      Code3,
      Body2,
      Code4,
      Body3,
      Code5,
      Body4,
      Code6,
      Body5,
      Code7,
      Body6,
      Code8,
      Body7,
      Code9,
      Body8,
      Code10,
      Body9,
      Code11,
      Body10,
      Code12,
      Body11,
      Code13);

   --  FLASH_CFPA
   type FLASH_CFPA_Peripheral
     (Discriminent : FLASH_CFPA_Disc := Code0)
   is record
      --  .
      HEADER                    : aliased HAL.UInt32;
      --  .
      VERSION                   : aliased HAL.UInt32;
      --  Secure firmware version (Monotonic counter)
      S_FW_Version              : aliased HAL.UInt32;
      --  Non-Secure firmware version (Monotonic counter)
      NS_FW_Version             : aliased HAL.UInt32;
      --  Image key revocation ID (Monotonic counter)
      IMAGE_KEY_REVOKE          : aliased HAL.UInt32;
      --  .
      ROTKH_REVOKE              : aliased ROTKH_REVOKE_Register;
      --  .
      VENDOR_USAGE              : aliased VENDOR_USAGE_Register;
      --  With TZ-M, the part can be sold by level 1 customers (secure code
      --  developer) to level-2 customers who develops non-secure code only. -
      --  In this scenario, or easy of development, Level-I customer releases
      --  the part to always allow non-secure debug. - To allow level-2
      --  customers to further seal the part DCFG_CC_SOCU_NS is used. - ROM
      --  will use this word to further restrict the debug access.
      DCFG_CC_SOCU_PIN          : aliased DCFG_CC_SOCU_PIN_Register;
      --  With TZ-M, the part can be sold by level 1 customers (secure code
      --  developer) to level-2 customers who develops non-secure code only. -
      --  In this scenario, or easy of development, Level-I customer releases
      --  the part to always allow non-secure debug. - To allow level-2
      --  customers to further seal the part DCFG_CC_SOCU_NS is used. - ROM
      --  will use this word to further restrict the debug access.
      DCFG_CC_SOCU_DFLT         : aliased DCFG_CC_SOCU_DFLT_Register;
      --  Enable FA mode. SET_FA_MODE Command should write 0xC33CA55A to this
      --  word to indicate boot ROM to enter FA mode.
      ENABLE_FA_MODE            : aliased HAL.UInt32;
      --  CMPA Page programming on going. This field shall be set to 0x5CC55AA5
      --  in the active CFPA page each time CMPA page programming is going on.
      --  It shall always be set to 0x00000000 in the CFPA scratch area.
      CMPA_PROG_IN_PROGRESS     : aliased HAL.UInt32;
      --  Customer Defined (Programable through ROM API)
      CUSTOMER_DEFINED          : aliased CUSTOMER_DEFINED_Registers;
      --  SHA256_DIGEST0 for DIGEST[31:0] SHA256_DIGEST1 for DIGEST[63:32]
      --  SHA256_DIGEST2 for DIGEST[95:64] SHA256_DIGEST3 for DIGEST[127:96]
      --  SHA256_DIGEST4 for DIGEST[159:128] SHA256_DIGEST5 for DIGEST[191:160]
      --  SHA256_DIGEST6 for DIGEST[223:192] SHA256_DIGEST7 for DIGEST[255:224]
      SHA256_DIGEST             : aliased SHA256_DIGEST_Registers;
      case Discriminent is
         when Code0 =>
            --  .
            PRINCE_REGION0_IV_CODE0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE0 : aliased HAL.UInt32;
         when Header0 =>
            --  .
            PRINCE_REGION0_IV_HEADER0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_HEADER0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_HEADER0 : aliased HAL.UInt32;
         when Code1 =>
            --  .
            PRINCE_REGION0_IV_CODE1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE1 : aliased HAL.UInt32;
         when Header1 =>
            --  .
            PRINCE_REGION0_IV_HEADER1 : aliased PRINCE_REGION0_IV_HEADER1_Register;
            --  .
            PRINCE_REGION1_IV_HEADER1 : aliased PRINCE_REGION1_IV_HEADER1_Register;
            --  .
            PRINCE_REGION2_IV_HEADER1 : aliased PRINCE_REGION2_IV_HEADER1_Register;
         when Body0 =>
            --  .
            PRINCE_REGION0_IV_BODY0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY0 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY0 : aliased HAL.UInt32;
         when Code2 =>
            --  .
            PRINCE_REGION0_IV_CODE2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE2 : aliased HAL.UInt32;
         when Body1 =>
            --  .
            PRINCE_REGION0_IV_BODY1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY1 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY1 : aliased HAL.UInt32;
         when Code3 =>
            --  .
            PRINCE_REGION0_IV_CODE3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE3 : aliased HAL.UInt32;
         when Body2 =>
            --  .
            PRINCE_REGION0_IV_BODY2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY2 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY2 : aliased HAL.UInt32;
         when Code4 =>
            --  .
            PRINCE_REGION0_IV_CODE4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE4 : aliased HAL.UInt32;
         when Body3 =>
            --  .
            PRINCE_REGION0_IV_BODY3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY3 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY3 : aliased HAL.UInt32;
         when Code5 =>
            --  .
            PRINCE_REGION0_IV_CODE5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE5 : aliased HAL.UInt32;
         when Body4 =>
            --  .
            PRINCE_REGION0_IV_BODY4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY4 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY4 : aliased HAL.UInt32;
         when Code6 =>
            --  .
            PRINCE_REGION0_IV_CODE6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE6 : aliased HAL.UInt32;
         when Body5 =>
            --  .
            PRINCE_REGION0_IV_BODY5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY5 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY5 : aliased HAL.UInt32;
         when Code7 =>
            --  .
            PRINCE_REGION0_IV_CODE7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE7 : aliased HAL.UInt32;
         when Body6 =>
            --  .
            PRINCE_REGION0_IV_BODY6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY6 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY6 : aliased HAL.UInt32;
         when Code8 =>
            --  .
            PRINCE_REGION0_IV_CODE8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE8 : aliased HAL.UInt32;
         when Body7 =>
            --  .
            PRINCE_REGION0_IV_BODY7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY7 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY7 : aliased HAL.UInt32;
         when Code9 =>
            --  .
            PRINCE_REGION0_IV_CODE9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE9 : aliased HAL.UInt32;
         when Body8 =>
            --  .
            PRINCE_REGION0_IV_BODY8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY8 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY8 : aliased HAL.UInt32;
         when Code10 =>
            --  .
            PRINCE_REGION0_IV_CODE10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE10 : aliased HAL.UInt32;
         when Body9 =>
            --  .
            PRINCE_REGION0_IV_BODY9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY9 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY9 : aliased HAL.UInt32;
         when Code11 =>
            --  .
            PRINCE_REGION0_IV_CODE11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE11 : aliased HAL.UInt32;
         when Body10 =>
            --  .
            PRINCE_REGION0_IV_BODY10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY10 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY10 : aliased HAL.UInt32;
         when Code12 =>
            --  .
            PRINCE_REGION0_IV_CODE12 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE12 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE12 : aliased HAL.UInt32;
         when Body11 =>
            --  .
            PRINCE_REGION0_IV_BODY11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_BODY11 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_BODY11 : aliased HAL.UInt32;
         when Code13 =>
            --  .
            PRINCE_REGION0_IV_CODE13 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION1_IV_CODE13 : aliased HAL.UInt32;
            --  .
            PRINCE_REGION2_IV_CODE13 : aliased HAL.UInt32;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for FLASH_CFPA_Peripheral use record
      HEADER                    at 16#0# range 0 .. 31;
      VERSION                   at 16#4# range 0 .. 31;
      S_FW_Version              at 16#8# range 0 .. 31;
      NS_FW_Version             at 16#C# range 0 .. 31;
      IMAGE_KEY_REVOKE          at 16#10# range 0 .. 31;
      ROTKH_REVOKE              at 16#18# range 0 .. 31;
      VENDOR_USAGE              at 16#1C# range 0 .. 31;
      DCFG_CC_SOCU_PIN          at 16#20# range 0 .. 31;
      DCFG_CC_SOCU_DFLT         at 16#24# range 0 .. 31;
      ENABLE_FA_MODE            at 16#28# range 0 .. 31;
      CMPA_PROG_IN_PROGRESS     at 16#2C# range 0 .. 31;
      CUSTOMER_DEFINED          at 16#100# range 0 .. 1791;
      SHA256_DIGEST             at 16#1E0# range 0 .. 255;
      PRINCE_REGION0_IV_CODE0   at 16#30# range 0 .. 31;
      PRINCE_REGION1_IV_CODE0   at 16#68# range 0 .. 31;
      PRINCE_REGION2_IV_CODE0   at 16#A0# range 0 .. 31;
      PRINCE_REGION0_IV_HEADER0 at 16#30# range 0 .. 31;
      PRINCE_REGION1_IV_HEADER0 at 16#68# range 0 .. 31;
      PRINCE_REGION2_IV_HEADER0 at 16#A0# range 0 .. 31;
      PRINCE_REGION0_IV_CODE1   at 16#34# range 0 .. 31;
      PRINCE_REGION1_IV_CODE1   at 16#6C# range 0 .. 31;
      PRINCE_REGION2_IV_CODE1   at 16#A4# range 0 .. 31;
      PRINCE_REGION0_IV_HEADER1 at 16#34# range 0 .. 31;
      PRINCE_REGION1_IV_HEADER1 at 16#6C# range 0 .. 31;
      PRINCE_REGION2_IV_HEADER1 at 16#A4# range 0 .. 31;
      PRINCE_REGION0_IV_BODY0   at 16#38# range 0 .. 31;
      PRINCE_REGION1_IV_BODY0   at 16#70# range 0 .. 31;
      PRINCE_REGION2_IV_BODY0   at 16#A8# range 0 .. 31;
      PRINCE_REGION0_IV_CODE2   at 16#38# range 0 .. 31;
      PRINCE_REGION1_IV_CODE2   at 16#70# range 0 .. 31;
      PRINCE_REGION2_IV_CODE2   at 16#A8# range 0 .. 31;
      PRINCE_REGION0_IV_BODY1   at 16#3C# range 0 .. 31;
      PRINCE_REGION1_IV_BODY1   at 16#74# range 0 .. 31;
      PRINCE_REGION2_IV_BODY1   at 16#AC# range 0 .. 31;
      PRINCE_REGION0_IV_CODE3   at 16#3C# range 0 .. 31;
      PRINCE_REGION1_IV_CODE3   at 16#74# range 0 .. 31;
      PRINCE_REGION2_IV_CODE3   at 16#AC# range 0 .. 31;
      PRINCE_REGION0_IV_BODY2   at 16#40# range 0 .. 31;
      PRINCE_REGION1_IV_BODY2   at 16#78# range 0 .. 31;
      PRINCE_REGION2_IV_BODY2   at 16#B0# range 0 .. 31;
      PRINCE_REGION0_IV_CODE4   at 16#40# range 0 .. 31;
      PRINCE_REGION1_IV_CODE4   at 16#78# range 0 .. 31;
      PRINCE_REGION2_IV_CODE4   at 16#B0# range 0 .. 31;
      PRINCE_REGION0_IV_BODY3   at 16#44# range 0 .. 31;
      PRINCE_REGION1_IV_BODY3   at 16#7C# range 0 .. 31;
      PRINCE_REGION2_IV_BODY3   at 16#B4# range 0 .. 31;
      PRINCE_REGION0_IV_CODE5   at 16#44# range 0 .. 31;
      PRINCE_REGION1_IV_CODE5   at 16#7C# range 0 .. 31;
      PRINCE_REGION2_IV_CODE5   at 16#B4# range 0 .. 31;
      PRINCE_REGION0_IV_BODY4   at 16#48# range 0 .. 31;
      PRINCE_REGION1_IV_BODY4   at 16#80# range 0 .. 31;
      PRINCE_REGION2_IV_BODY4   at 16#B8# range 0 .. 31;
      PRINCE_REGION0_IV_CODE6   at 16#48# range 0 .. 31;
      PRINCE_REGION1_IV_CODE6   at 16#80# range 0 .. 31;
      PRINCE_REGION2_IV_CODE6   at 16#B8# range 0 .. 31;
      PRINCE_REGION0_IV_BODY5   at 16#4C# range 0 .. 31;
      PRINCE_REGION1_IV_BODY5   at 16#84# range 0 .. 31;
      PRINCE_REGION2_IV_BODY5   at 16#BC# range 0 .. 31;
      PRINCE_REGION0_IV_CODE7   at 16#4C# range 0 .. 31;
      PRINCE_REGION1_IV_CODE7   at 16#84# range 0 .. 31;
      PRINCE_REGION2_IV_CODE7   at 16#BC# range 0 .. 31;
      PRINCE_REGION0_IV_BODY6   at 16#50# range 0 .. 31;
      PRINCE_REGION1_IV_BODY6   at 16#88# range 0 .. 31;
      PRINCE_REGION2_IV_BODY6   at 16#C0# range 0 .. 31;
      PRINCE_REGION0_IV_CODE8   at 16#50# range 0 .. 31;
      PRINCE_REGION1_IV_CODE8   at 16#88# range 0 .. 31;
      PRINCE_REGION2_IV_CODE8   at 16#C0# range 0 .. 31;
      PRINCE_REGION0_IV_BODY7   at 16#54# range 0 .. 31;
      PRINCE_REGION1_IV_BODY7   at 16#8C# range 0 .. 31;
      PRINCE_REGION2_IV_BODY7   at 16#C4# range 0 .. 31;
      PRINCE_REGION0_IV_CODE9   at 16#54# range 0 .. 31;
      PRINCE_REGION1_IV_CODE9   at 16#8C# range 0 .. 31;
      PRINCE_REGION2_IV_CODE9   at 16#C4# range 0 .. 31;
      PRINCE_REGION0_IV_BODY8   at 16#58# range 0 .. 31;
      PRINCE_REGION1_IV_BODY8   at 16#90# range 0 .. 31;
      PRINCE_REGION2_IV_BODY8   at 16#C8# range 0 .. 31;
      PRINCE_REGION0_IV_CODE10  at 16#58# range 0 .. 31;
      PRINCE_REGION1_IV_CODE10  at 16#90# range 0 .. 31;
      PRINCE_REGION2_IV_CODE10  at 16#C8# range 0 .. 31;
      PRINCE_REGION0_IV_BODY9   at 16#5C# range 0 .. 31;
      PRINCE_REGION1_IV_BODY9   at 16#94# range 0 .. 31;
      PRINCE_REGION2_IV_BODY9   at 16#CC# range 0 .. 31;
      PRINCE_REGION0_IV_CODE11  at 16#5C# range 0 .. 31;
      PRINCE_REGION1_IV_CODE11  at 16#94# range 0 .. 31;
      PRINCE_REGION2_IV_CODE11  at 16#CC# range 0 .. 31;
      PRINCE_REGION0_IV_BODY10  at 16#60# range 0 .. 31;
      PRINCE_REGION1_IV_BODY10  at 16#98# range 0 .. 31;
      PRINCE_REGION2_IV_BODY10  at 16#D0# range 0 .. 31;
      PRINCE_REGION0_IV_CODE12  at 16#60# range 0 .. 31;
      PRINCE_REGION1_IV_CODE12  at 16#98# range 0 .. 31;
      PRINCE_REGION2_IV_CODE12  at 16#D0# range 0 .. 31;
      PRINCE_REGION0_IV_BODY11  at 16#64# range 0 .. 31;
      PRINCE_REGION1_IV_BODY11  at 16#9C# range 0 .. 31;
      PRINCE_REGION2_IV_BODY11  at 16#D4# range 0 .. 31;
      PRINCE_REGION0_IV_CODE13  at 16#64# range 0 .. 31;
      PRINCE_REGION1_IV_CODE13  at 16#9C# range 0 .. 31;
      PRINCE_REGION2_IV_CODE13  at 16#D4# range 0 .. 31;
   end record;

   --  FLASH_CFPA
   FLASH_CFPA0_Periph : aliased FLASH_CFPA_Peripheral
     with Import, Address => System'To_Address (16#9E000#);

   --  FLASH_CFPA
   FLASH_CFPA1_Periph : aliased FLASH_CFPA_Peripheral
     with Import, Address => System'To_Address (16#9E200#);

   --  FLASH_CFPA
   FLASH_CFPA_SCRATCH_Periph : aliased FLASH_CFPA_Peripheral
     with Import, Address => System'To_Address (16#9DE00#);

end NXP_SVD.FLASH_CFPA;
