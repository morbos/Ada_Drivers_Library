--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.FLASH_CMPA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Default ISP mode:
   type BOOT_CFG_DEFAULT_ISP_MODE_Field is
     (
      --  Auto ISP
      Value_0,
      --  USB_HID_MSC
      Value_1,
      --  SPI Slave ISP
      Value_2,
      --  I2C Slave ISP
      Value_3,
      --  Disable ISP fall through
      Value_7)
     with Size => 3;
   for BOOT_CFG_DEFAULT_ISP_MODE_Field use
     (Value_0 => 0,
      Value_1 => 1,
      Value_2 => 2,
      Value_3 => 3,
      Value_7 => 7);

   --  Core clock:
   type BOOT_CFG_BOOT_SPEED_Field is
     (
      --  Defined by NMPA.SYSTEM_SPEED_CODE
      Value_0,
      --  48MHz FRO
      Value_1,
      --  96MHz FRO
      Value_2)
     with Size => 2;
   for BOOT_CFG_BOOT_SPEED_Field use
     (Value_0 => 0,
      Value_1 => 1,
      Value_2 => 2);

   subtype BOOT_CFG_BOOT_FAILURE_PIN_Field is HAL.UInt8;

   --  .
   type BOOT_CFG_Register is record
      --  unspecified
      Reserved_0_3     : HAL.UInt4 := 16#0#;
      --  Default ISP mode:
      DEFAULT_ISP_MODE : BOOT_CFG_DEFAULT_ISP_MODE_Field :=
                          NXP_SVD.FLASH_CMPA.Value_0;
      --  Core clock:
      BOOT_SPEED       : BOOT_CFG_BOOT_SPEED_Field :=
                          NXP_SVD.FLASH_CMPA.Value_0;
      --  unspecified
      Reserved_9_23    : HAL.UInt15 := 16#0#;
      --  GPIO port and pin number to use for indicating failure reason. The
      --  toggle rate of the pin is used to decode the error type. [2:0] -
      --  Defines GPIO port [7:3] - Defines GPIO pin
      BOOT_FAILURE_PIN : BOOT_CFG_BOOT_FAILURE_PIN_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BOOT_CFG_Register use record
      Reserved_0_3     at 0 range 0 .. 3;
      DEFAULT_ISP_MODE at 0 range 4 .. 6;
      BOOT_SPEED       at 0 range 7 .. 8;
      Reserved_9_23    at 0 range 9 .. 23;
      BOOT_FAILURE_PIN at 0 range 24 .. 31;
   end record;

   subtype SPI_FLASH_CFG_SPI_RECOVERY_BOOT_EN_Field is HAL.UInt5;

   --  .
   type SPI_FLASH_CFG_Register is record
      --  SPI flash recovery boot is enabled, if non-zero value is written to
      --  this field.
      SPI_RECOVERY_BOOT_EN : SPI_FLASH_CFG_SPI_RECOVERY_BOOT_EN_Field :=
                              16#0#;
      --  unspecified
      Reserved_5_31        : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI_FLASH_CFG_Register use record
      SPI_RECOVERY_BOOT_EN at 0 range 0 .. 4;
      Reserved_5_31        at 0 range 5 .. 31;
   end record;

   subtype USB_ID_USB_VENDOR_ID_Field is HAL.UInt16;
   subtype USB_ID_USB_PRODUCT_ID_Field is HAL.UInt16;

   --  .
   type USB_ID_Register is record
      --  .
      USB_VENDOR_ID  : USB_ID_USB_VENDOR_ID_Field := 16#0#;
      --  .
      USB_PRODUCT_ID : USB_ID_USB_PRODUCT_ID_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_ID_Register use record
      USB_VENDOR_ID  at 0 range 0 .. 15;
      USB_PRODUCT_ID at 0 range 16 .. 31;
   end record;

   --  Non Secure non-invasive debug enable
   type CC_SOCU_PIN_NIDEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_NIDEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Non Secure debug enable
   type CC_SOCU_PIN_DBGEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_DBGEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Secure non-invasive debug enable
   type CC_SOCU_PIN_SPNIDEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_SPNIDEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Secure invasive debug enable
   type CC_SOCU_PIN_SPIDEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_SPIDEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  JTAG TAP enable
   type CC_SOCU_PIN_TAPEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_TAPEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  CPU1 (Micro cortex M33) invasive debug enable
   type CC_SOCU_PIN_CPU1_DBGEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_CPU1_DBGEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  ISP Boot Command enable
   type CC_SOCU_PIN_ISP_CMD_EN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_ISP_CMD_EN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  FA Command enable
   type CC_SOCU_PIN_FA_CMD_EN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_FA_CMD_EN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Flash Mass Erase Command enable
   type CC_SOCU_PIN_ME_CMD_EN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_ME_CMD_EN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  CPU1 (Micro cortex M33) non-invasive debug enable
   type CC_SOCU_PIN_CPU1_NIDEN_Field is
     (
      --  Use DAP to enable
      Value_0,
      --  Fixed state
      Value_1)
     with Size => 1;
   for CC_SOCU_PIN_CPU1_NIDEN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   subtype CC_SOCU_PIN_INVERSE_VALUE_Field is HAL.UInt16;

   --  .
   type CC_SOCU_PIN_Register is record
      --  Non Secure non-invasive debug enable
      NIDEN          : CC_SOCU_PIN_NIDEN_Field := NXP_SVD.FLASH_CMPA.Value_0;
      --  Non Secure debug enable
      DBGEN          : CC_SOCU_PIN_DBGEN_Field := NXP_SVD.FLASH_CMPA.Value_0;
      --  Secure non-invasive debug enable
      SPNIDEN        : CC_SOCU_PIN_SPNIDEN_Field :=
                        NXP_SVD.FLASH_CMPA.Value_0;
      --  Secure invasive debug enable
      SPIDEN         : CC_SOCU_PIN_SPIDEN_Field := NXP_SVD.FLASH_CMPA.Value_0;
      --  JTAG TAP enable
      TAPEN          : CC_SOCU_PIN_TAPEN_Field := NXP_SVD.FLASH_CMPA.Value_0;
      --  CPU1 (Micro cortex M33) invasive debug enable
      CPU1_DBGEN     : CC_SOCU_PIN_CPU1_DBGEN_Field :=
                        NXP_SVD.FLASH_CMPA.Value_0;
      --  ISP Boot Command enable
      ISP_CMD_EN     : CC_SOCU_PIN_ISP_CMD_EN_Field :=
                        NXP_SVD.FLASH_CMPA.Value_0;
      --  FA Command enable
      FA_CMD_EN      : CC_SOCU_PIN_FA_CMD_EN_Field :=
                        NXP_SVD.FLASH_CMPA.Value_0;
      --  Flash Mass Erase Command enable
      ME_CMD_EN      : CC_SOCU_PIN_ME_CMD_EN_Field :=
                        NXP_SVD.FLASH_CMPA.Value_0;
      --  CPU1 (Micro cortex M33) non-invasive debug enable
      CPU1_NIDEN     : CC_SOCU_PIN_CPU1_NIDEN_Field :=
                        NXP_SVD.FLASH_CMPA.Value_0;
      --  unspecified
      Reserved_10_14 : HAL.UInt5 := 16#0#;
      --  Enforce UUID match during Debug authentication.
      UUID_CHECK     : Boolean := False;
      --  inverse value of bits [15:0]
      INVERSE_VALUE  : CC_SOCU_PIN_INVERSE_VALUE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_SOCU_PIN_Register use record
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
   type CC_SOCU_DFLT_NIDEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_NIDEN_Field use
     (Disable => 0,
      Enable => 1);

   --  Non Secure debug fixed state
   type CC_SOCU_DFLT_DBGEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_DBGEN_Field use
     (Disable => 0,
      Enable => 1);

   --  Secure non-invasive debug fixed state
   type CC_SOCU_DFLT_SPNIDEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_SPNIDEN_Field use
     (Disable => 0,
      Enable => 1);

   --  Secure invasive debug fixed state
   type CC_SOCU_DFLT_SPIDEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_SPIDEN_Field use
     (Disable => 0,
      Enable => 1);

   --  JTAG TAP fixed state
   type CC_SOCU_DFLT_TAPEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_TAPEN_Field use
     (Disable => 0,
      Enable => 1);

   --  CPU1 (Micro cortex M33) invasive debug fixed state
   type CC_SOCU_DFLT_CPU1_DBGEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_CPU1_DBGEN_Field use
     (Disable => 0,
      Enable => 1);

   --  ISP Boot Command fixed state
   type CC_SOCU_DFLT_ISP_CMD_EN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_ISP_CMD_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  FA Command fixed state
   type CC_SOCU_DFLT_FA_CMD_EN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_FA_CMD_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  Flash Mass Erase Command fixed state
   type CC_SOCU_DFLT_ME_CMD_EN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_ME_CMD_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  CPU1 (Micro cortex M33) non-invasive debug fixed state
   type CC_SOCU_DFLT_CPU1_NIDEN_Field is
     (
      --  Disable
      Disable,
      --  Enable
      Enable)
     with Size => 1;
   for CC_SOCU_DFLT_CPU1_NIDEN_Field use
     (Disable => 0,
      Enable => 1);

   subtype CC_SOCU_DFLT_INVERSE_VALUE_Field is HAL.UInt16;

   --  .
   type CC_SOCU_DFLT_Register is record
      --  Non Secure non-invasive debug fixed state
      NIDEN          : CC_SOCU_DFLT_NIDEN_Field := NXP_SVD.FLASH_CMPA.Disable;
      --  Non Secure debug fixed state
      DBGEN          : CC_SOCU_DFLT_DBGEN_Field := NXP_SVD.FLASH_CMPA.Disable;
      --  Secure non-invasive debug fixed state
      SPNIDEN        : CC_SOCU_DFLT_SPNIDEN_Field :=
                        NXP_SVD.FLASH_CMPA.Disable;
      --  Secure invasive debug fixed state
      SPIDEN         : CC_SOCU_DFLT_SPIDEN_Field :=
                        NXP_SVD.FLASH_CMPA.Disable;
      --  JTAG TAP fixed state
      TAPEN          : CC_SOCU_DFLT_TAPEN_Field := NXP_SVD.FLASH_CMPA.Disable;
      --  CPU1 (Micro cortex M33) invasive debug fixed state
      CPU1_DBGEN     : CC_SOCU_DFLT_CPU1_DBGEN_Field :=
                        NXP_SVD.FLASH_CMPA.Disable;
      --  ISP Boot Command fixed state
      ISP_CMD_EN     : CC_SOCU_DFLT_ISP_CMD_EN_Field :=
                        NXP_SVD.FLASH_CMPA.Disable;
      --  FA Command fixed state
      FA_CMD_EN      : CC_SOCU_DFLT_FA_CMD_EN_Field :=
                        NXP_SVD.FLASH_CMPA.Disable;
      --  Flash Mass Erase Command fixed state
      ME_CMD_EN      : CC_SOCU_DFLT_ME_CMD_EN_Field :=
                        NXP_SVD.FLASH_CMPA.Disable;
      --  CPU1 (Micro cortex M33) non-invasive debug fixed state
      CPU1_NIDEN     : CC_SOCU_DFLT_CPU1_NIDEN_Field :=
                        NXP_SVD.FLASH_CMPA.Disable;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  inverse value of bits [15:0]
      INVERSE_VALUE  : CC_SOCU_DFLT_INVERSE_VALUE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_SOCU_DFLT_Register use record
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

   subtype VENDOR_USAGE_VENDOR_USAGE_Field is HAL.UInt16;

   --  .
   type VENDOR_USAGE_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  Upper 16 bits of vendor usage field defined in DAP. Lower 16-bits
      --  come from customer field area.
      VENDOR_USAGE  : VENDOR_USAGE_VENDOR_USAGE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for VENDOR_USAGE_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      VENDOR_USAGE  at 0 range 16 .. 31;
   end record;

   subtype SECURE_BOOT_CFG_RSA4K_Field is HAL.UInt2;
   subtype SECURE_BOOT_CFG_DICE_ENC_NXP_CFG_Field is HAL.UInt2;
   subtype SECURE_BOOT_CFG_DICE_CUST_CFG_Field is HAL.UInt2;
   subtype SECURE_BOOT_CFG_SKIP_DICE_Field is HAL.UInt2;
   subtype SECURE_BOOT_CFG_TZM_IMAGE_TYPE_Field is HAL.UInt2;
   subtype SECURE_BOOT_CFG_BLOCK_SET_KEY_Field is HAL.UInt2;
   subtype SECURE_BOOT_CFG_BLOCK_ENROLL_Field is HAL.UInt2;
   subtype SECURE_BOOT_CFG_DICE_INC_SEC_EPOCH_Field is HAL.UInt2;
   subtype SECURE_BOOT_CFG_SEC_BOOT_EN_Field is HAL.UInt2;

   --  .
   type SECURE_BOOT_CFG_Register is record
      --  Use RSA4096 keys only. 00- RSA2048 keys 01, 10, 11 - RSA4096 keys
      RSA4K              : SECURE_BOOT_CFG_RSA4K_Field := 16#0#;
      --  Include NXP area in DICE computation. 00 - not included 01, 10, 11 -
      --  included
      DICE_ENC_NXP_CFG   : SECURE_BOOT_CFG_DICE_ENC_NXP_CFG_Field := 16#0#;
      --  Include Customer factory area (including keys) in DICE computation.
      --  00 - not included 01, 10, 11 - included
      DICE_CUST_CFG      : SECURE_BOOT_CFG_DICE_CUST_CFG_Field := 16#0#;
      --  Skip DICE computation. 00 - Enable DICE 01,10,11 - Disable DICE
      SKIP_DICE          : SECURE_BOOT_CFG_SKIP_DICE_Field := 16#0#;
      --  TrustZone-M mode. 00 - TZM mode in image header. 01 - Disable TZ-M.
      --  Boots to NonSecure. 10 - TZ-M enable boots to secure mode. 11 -
      --  Preset TZM checker from image header.
      TZM_IMAGE_TYPE     : SECURE_BOOT_CFG_TZM_IMAGE_TYPE_Field := 16#0#;
      --  Block PUF key code generation. 00 - Enable Key code generation 01,
      --  10, 11 - Disable key code generation
      BLOCK_SET_KEY      : SECURE_BOOT_CFG_BLOCK_SET_KEY_Field := 16#0#;
      --  Block PUF enrollement. 00 - Enable enrollment mode 01, 10, 11 -
      --  Disable further enrollmnet
      BLOCK_ENROLL       : SECURE_BOOT_CFG_BLOCK_ENROLL_Field := 16#0#;
      --  Include security EPOCH in DICE
      DICE_INC_SEC_EPOCH : SECURE_BOOT_CFG_DICE_INC_SEC_EPOCH_Field := 16#0#;
      --  unspecified
      Reserved_16_29     : HAL.UInt14 := 16#0#;
      --  Secure boot enable. 00 - Plain image (internal flash with or without
      --  CRC) 01, 10, 11 - Boot signed images. (internal flash, RSA signed)
      SEC_BOOT_EN        : SECURE_BOOT_CFG_SEC_BOOT_EN_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECURE_BOOT_CFG_Register use record
      RSA4K              at 0 range 0 .. 1;
      DICE_ENC_NXP_CFG   at 0 range 2 .. 3;
      DICE_CUST_CFG      at 0 range 4 .. 5;
      SKIP_DICE          at 0 range 6 .. 7;
      TZM_IMAGE_TYPE     at 0 range 8 .. 9;
      BLOCK_SET_KEY      at 0 range 10 .. 11;
      BLOCK_ENROLL       at 0 range 12 .. 13;
      DICE_INC_SEC_EPOCH at 0 range 14 .. 15;
      Reserved_16_29     at 0 range 16 .. 29;
      SEC_BOOT_EN        at 0 range 30 .. 31;
   end record;

   subtype PRINCE_BASE_ADDR_ADDR0_PRG_Field is HAL.UInt4;
   subtype PRINCE_BASE_ADDR_ADDR1_PRG_Field is HAL.UInt4;
   subtype PRINCE_BASE_ADDR_ADDR2_PRG_Field is HAL.UInt4;
   --  PRINCE_BASE_ADDR_LOCK_REG array element
   subtype PRINCE_BASE_ADDR_LOCK_REG_Element is HAL.UInt2;

   --  PRINCE_BASE_ADDR_LOCK_REG array
   type PRINCE_BASE_ADDR_LOCK_REG_Field_Array is array (0 .. 2)
     of PRINCE_BASE_ADDR_LOCK_REG_Element
     with Component_Size => 2, Size => 6;

   --  Type definition for PRINCE_BASE_ADDR_LOCK_REG
   type PRINCE_BASE_ADDR_LOCK_REG_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LOCK_REG as a value
            Val : HAL.UInt6;
         when True =>
            --  LOCK_REG as an array
            Arr : PRINCE_BASE_ADDR_LOCK_REG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for PRINCE_BASE_ADDR_LOCK_REG_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   subtype PRINCE_BASE_ADDR_REG0_ERASE_CHECK_EN_Field is HAL.UInt2;
   subtype PRINCE_BASE_ADDR_REG1_ERASE_CHECK_EN_Field is HAL.UInt2;
   subtype PRINCE_BASE_ADDR_REG2_ERASE_CHECK_EN_Field is HAL.UInt2;

   --  .
   type PRINCE_BASE_ADDR_Register is record
      --  Programmable portion of the base address of region 0.
      ADDR0_PRG           : PRINCE_BASE_ADDR_ADDR0_PRG_Field := 16#0#;
      --  Programmable portion of the base address of region 1.
      ADDR1_PRG           : PRINCE_BASE_ADDR_ADDR1_PRG_Field := 16#0#;
      --  Programmable portion of the base address of region 2.
      ADDR2_PRG           : PRINCE_BASE_ADDR_ADDR2_PRG_Field := 16#0#;
      --  unspecified
      Reserved_12_15      : HAL.UInt4 := 16#0#;
      --  Lock PRINCE region0 settings. 00 - Region is not locked. 01, 10, 11 -
      --  Region is locked.
      LOCK_REG            : PRINCE_BASE_ADDR_LOCK_REG_Field :=
                             (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_23      : HAL.UInt2 := 16#0#;
      --  For PRINCE region0 enable checking whether all encrypted pages are
      --  erased together. 00 - Check is disabled. 01, 10, 11 - Check is
      --  enabled.
      REG0_ERASE_CHECK_EN : PRINCE_BASE_ADDR_REG0_ERASE_CHECK_EN_Field :=
                             16#0#;
      --  For PRINCE region1 enable checking whether all encrypted pages are
      --  erased together. 00 - Check is disabled. 01, 10, 11 - Check is
      --  enabled.
      REG1_ERASE_CHECK_EN : PRINCE_BASE_ADDR_REG1_ERASE_CHECK_EN_Field :=
                             16#0#;
      --  For PRINCE region2 enable checking whether all encrypted pages are
      --  erased together. 00 - Check is disabled. 01, 10, 11 - Check is
      --  enabled.
      REG2_ERASE_CHECK_EN : PRINCE_BASE_ADDR_REG2_ERASE_CHECK_EN_Field :=
                             16#0#;
      --  unspecified
      Reserved_30_31      : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRINCE_BASE_ADDR_Register use record
      ADDR0_PRG           at 0 range 0 .. 3;
      ADDR1_PRG           at 0 range 4 .. 7;
      ADDR2_PRG           at 0 range 8 .. 11;
      Reserved_12_15      at 0 range 12 .. 15;
      LOCK_REG            at 0 range 16 .. 21;
      Reserved_22_23      at 0 range 22 .. 23;
      REG0_ERASE_CHECK_EN at 0 range 24 .. 25;
      REG1_ERASE_CHECK_EN at 0 range 26 .. 27;
      REG2_ERASE_CHECK_EN at 0 range 28 .. 29;
      Reserved_30_31      at 0 range 30 .. 31;
   end record;

   subtype XTAL_32KHZ_CAPABANK_TRIM_XTAL_LOAD_CAP_IEC_PF_X100_Field is
     HAL.UInt10;
   subtype XTAL_32KHZ_CAPABANK_TRIM_PCB_XIN_PARA_CAP_PF_X100_Field is
     HAL.UInt10;
   subtype XTAL_32KHZ_CAPABANK_TRIM_PCB_XOUT_PARA_CAP_PF_X100_Field is
     HAL.UInt10;

   --  Xtal 32kHz capabank triming.
   type XTAL_32KHZ_CAPABANK_TRIM_Register is record
      --  0 : Capa Bank trimmings not valid. Default trimmings value are used.
      --  1 : Capa Bank trimmings valid.
      TRIM_VALID                : Boolean := False;
      --  Load capacitance, pF x 100. For example, 6pF becomes 600.
      XTAL_LOAD_CAP_IEC_PF_X100 : XTAL_32KHZ_CAPABANK_TRIM_XTAL_LOAD_CAP_IEC_PF_X100_Field :=
                                   16#0#;
      --  PCB XIN parasitic capacitance, pF x 100. For example, 6pF becomes
      --  600.
      PCB_XIN_PARA_CAP_PF_X100  : XTAL_32KHZ_CAPABANK_TRIM_PCB_XIN_PARA_CAP_PF_X100_Field :=
                                   16#0#;
      --  PCB XOUT parasitic capacitance, pF x 100. For example, 6pF becomes
      --  600.
      PCB_XOUT_PARA_CAP_PF_X100 : XTAL_32KHZ_CAPABANK_TRIM_PCB_XOUT_PARA_CAP_PF_X100_Field :=
                                   16#0#;
      --  unspecified
      Reserved_31_31            : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XTAL_32KHZ_CAPABANK_TRIM_Register use record
      TRIM_VALID                at 0 range 0 .. 0;
      XTAL_LOAD_CAP_IEC_PF_X100 at 0 range 1 .. 10;
      PCB_XIN_PARA_CAP_PF_X100  at 0 range 11 .. 20;
      PCB_XOUT_PARA_CAP_PF_X100 at 0 range 21 .. 30;
      Reserved_31_31            at 0 range 31 .. 31;
   end record;

   subtype XTAL_16MHZ_CAPABANK_TRIM_XTAL_LOAD_CAP_IEC_PF_X100_Field is
     HAL.UInt10;
   subtype XTAL_16MHZ_CAPABANK_TRIM_PCB_XIN_PARA_CAP_PF_X100_Field is
     HAL.UInt10;
   subtype XTAL_16MHZ_CAPABANK_TRIM_PCB_XOUT_PARA_CAP_PF_X100_Field is
     HAL.UInt10;

   --  Xtal 16MHz capabank triming.
   type XTAL_16MHZ_CAPABANK_TRIM_Register is record
      --  0 : Capa Bank trimmings not valid. Default trimmings value are used.
      --  1 : Capa Bank trimmings valid.
      TRIM_VALID                : Boolean := False;
      --  Load capacitance, pF x 100. For example, 6pF becomes 600.
      XTAL_LOAD_CAP_IEC_PF_X100 : XTAL_16MHZ_CAPABANK_TRIM_XTAL_LOAD_CAP_IEC_PF_X100_Field :=
                                   16#0#;
      --  PCB XIN parasitic capacitance, pF x 100. For example, 6pF becomes
      --  600.
      PCB_XIN_PARA_CAP_PF_X100  : XTAL_16MHZ_CAPABANK_TRIM_PCB_XIN_PARA_CAP_PF_X100_Field :=
                                   16#0#;
      --  PCB XOUT parasitic capacitance, pF x 100. For example, 6pF becomes
      --  600.
      PCB_XOUT_PARA_CAP_PF_X100 : XTAL_16MHZ_CAPABANK_TRIM_PCB_XOUT_PARA_CAP_PF_X100_Field :=
                                   16#0#;
      --  unspecified
      Reserved_31_31            : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XTAL_16MHZ_CAPABANK_TRIM_Register use record
      TRIM_VALID                at 0 range 0 .. 0;
      XTAL_LOAD_CAP_IEC_PF_X100 at 0 range 1 .. 10;
      PCB_XIN_PARA_CAP_PF_X100  at 0 range 11 .. 20;
      PCB_XOUT_PARA_CAP_PF_X100 at 0 range 21 .. 30;
      Reserved_31_31            at 0 range 31 .. 31;
   end record;

   --  ROTKH0 for Root of Trust Keys Table hash[255:224] ROTKH1 for Root of Trust Keys Table hash[223:192] ROTKH2 for Root of Trust Keys Table hash[191:160] ROTKH3 for Root of Trust Keys Table hash[159:128] ROTKH4 for Root of Trust Keys Table hash[127:96] ROTKH5 for Root of Trust Keys Table hash[95:64] ROTKH6 for Root of Trust Keys Table hash[63:32] ROTKH7 for Root of Trust Keys Table hash[31:0]

   --  ROTKH0 for Root of Trust Keys Table hash[255:224] ROTKH1 for Root of
   --  Trust Keys Table hash[223:192] ROTKH2 for Root of Trust Keys Table
   --  hash[191:160] ROTKH3 for Root of Trust Keys Table hash[159:128] ROTKH4
   --  for Root of Trust Keys Table hash[127:96] ROTKH5 for Root of Trust Keys
   --  Table hash[95:64] ROTKH6 for Root of Trust Keys Table hash[63:32] ROTKH7
   --  for Root of Trust Keys Table hash[31:0]
   type ROTKH_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

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

   --  FLASH_CMPA
   type FLASH_CMPA_Peripheral is record
      --  .
      BOOT_CFG                 : aliased BOOT_CFG_Register;
      --  .
      SPI_FLASH_CFG            : aliased SPI_FLASH_CFG_Register;
      --  .
      USB_ID                   : aliased USB_ID_Register;
      --  .
      SDIO_CFG                 : aliased HAL.UInt32;
      --  .
      CC_SOCU_PIN              : aliased CC_SOCU_PIN_Register;
      --  .
      CC_SOCU_DFLT             : aliased CC_SOCU_DFLT_Register;
      --  .
      VENDOR_USAGE             : aliased VENDOR_USAGE_Register;
      --  .
      SECURE_BOOT_CFG          : aliased SECURE_BOOT_CFG_Register;
      --  .
      PRINCE_BASE_ADDR         : aliased PRINCE_BASE_ADDR_Register;
      --  Region 0, sub-region enable
      PRINCE_SR_0              : aliased HAL.UInt32;
      --  Region 1, sub-region enable
      PRINCE_SR_1              : aliased HAL.UInt32;
      --  Region 2, sub-region enable
      PRINCE_SR_2              : aliased HAL.UInt32;
      --  Xtal 32kHz capabank triming.
      XTAL_32KHZ_CAPABANK_TRIM : aliased XTAL_32KHZ_CAPABANK_TRIM_Register;
      --  Xtal 16MHz capabank triming.
      XTAL_16MHZ_CAPABANK_TRIM : aliased XTAL_16MHZ_CAPABANK_TRIM_Register;
      --  ROTKH0 for Root of Trust Keys Table hash[255:224] ROTKH1 for Root of
      --  Trust Keys Table hash[223:192] ROTKH2 for Root of Trust Keys Table
      --  hash[191:160] ROTKH3 for Root of Trust Keys Table hash[159:128]
      --  ROTKH4 for Root of Trust Keys Table hash[127:96] ROTKH5 for Root of
      --  Trust Keys Table hash[95:64] ROTKH6 for Root of Trust Keys Table
      --  hash[63:32] ROTKH7 for Root of Trust Keys Table hash[31:0]
      ROTKH                    : aliased ROTKH_Registers;
      --  Customer Defined (Programable through ROM API)
      CUSTOMER_DEFINED         : aliased CUSTOMER_DEFINED_Registers;
      --  SHA256_DIGEST0 for DIGEST[31:0] SHA256_DIGEST1 for DIGEST[63:32]
      --  SHA256_DIGEST2 for DIGEST[95:64] SHA256_DIGEST3 for DIGEST[127:96]
      --  SHA256_DIGEST4 for DIGEST[159:128] SHA256_DIGEST5 for DIGEST[191:160]
      --  SHA256_DIGEST6 for DIGEST[223:192] SHA256_DIGEST7 for DIGEST[255:224]
      SHA256_DIGEST            : aliased SHA256_DIGEST_Registers;
   end record
     with Volatile;

   for FLASH_CMPA_Peripheral use record
      BOOT_CFG                 at 16#0# range 0 .. 31;
      SPI_FLASH_CFG            at 16#4# range 0 .. 31;
      USB_ID                   at 16#8# range 0 .. 31;
      SDIO_CFG                 at 16#C# range 0 .. 31;
      CC_SOCU_PIN              at 16#10# range 0 .. 31;
      CC_SOCU_DFLT             at 16#14# range 0 .. 31;
      VENDOR_USAGE             at 16#18# range 0 .. 31;
      SECURE_BOOT_CFG          at 16#1C# range 0 .. 31;
      PRINCE_BASE_ADDR         at 16#20# range 0 .. 31;
      PRINCE_SR_0              at 16#24# range 0 .. 31;
      PRINCE_SR_1              at 16#28# range 0 .. 31;
      PRINCE_SR_2              at 16#2C# range 0 .. 31;
      XTAL_32KHZ_CAPABANK_TRIM at 16#30# range 0 .. 31;
      XTAL_16MHZ_CAPABANK_TRIM at 16#34# range 0 .. 31;
      ROTKH                    at 16#50# range 0 .. 255;
      CUSTOMER_DEFINED         at 16#100# range 0 .. 1791;
      SHA256_DIGEST            at 16#1E0# range 0 .. 255;
   end record;

   --  FLASH_CMPA
   FLASH_CMPA_Periph : aliased FLASH_CMPA_Peripheral
     with Import, Address => System'To_Address (16#9E400#);

end NXP_SVD.FLASH_CMPA;
