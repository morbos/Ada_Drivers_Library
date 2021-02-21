--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.PRINCE is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Encryption Enable.
   type ENC_ENABLE_EN_Field is
     (
      --  Encryption of writes to the flash controller DATAW* registers is
      --  disabled.
      Disabled,
      --  Encryption of writes to the flash controller DATAW* registers is
      --  enabled.
      Enabled)
     with Size => 1;
   for ENC_ENABLE_EN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Encryption Enable register
   type ENC_ENABLE_Register is record
      --  Encryption Enable.
      EN            : ENC_ENABLE_EN_Field := NXP_SVD.PRINCE.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENC_ENABLE_Register use record
      EN            at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Lock Region 0 registers.
   type LOCK_LOCKREG0_Field is
     (
      --  Disabled. IV_LSB0, IV_MSB0, BASE_ADDR0, and SR_ENABLE0 are writable..
      Disabled,
      --  Enabled. IV_LSB0, IV_MSB0, BASE_ADDR0, and SR_ENABLE0 are not
      --  writable..
      Enabled)
     with Size => 1;
   for LOCK_LOCKREG0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Lock Region 1 registers.
   type LOCK_LOCKREG1_Field is
     (
      --  Disabled. IV_LSB1, IV_MSB1, BASE_ADDR1, and SR_ENABLE1 are writable..
      Disabled,
      --  Enabled. IV_LSB1, IV_MSB1, BASE_ADDR1, and SR_ENABLE1 are not
      --  writable..
      Enabled)
     with Size => 1;
   for LOCK_LOCKREG1_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Lock Region 2 registers.
   type LOCK_LOCKREG2_Field is
     (
      --  Disabled. IV_LSB2, IV_MSB2, BASE_ADDR2, and SR_ENABLE2 are writable..
      Disabled,
      --  Enabled. IV_LSB2, IV_MSB2, BASE_ADDR2, and SR_ENABLE2 are not
      --  writable..
      Enabled)
     with Size => 1;
   for LOCK_LOCKREG2_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Lock the Mask registers.
   type LOCK_LOCKMASK_Field is
     (
      --  Disabled. MASK_LSB, and MASK_MSB are writable..
      Disabled,
      --  Enabled. MASK_LSB, and MASK_MSB are not writable..
      Enabled)
     with Size => 1;
   for LOCK_LOCKMASK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Lock register
   type LOCK_Register is record
      --  Lock Region 0 registers.
      LOCKREG0      : LOCK_LOCKREG0_Field := NXP_SVD.PRINCE.Disabled;
      --  Lock Region 1 registers.
      LOCKREG1      : LOCK_LOCKREG1_Field := NXP_SVD.PRINCE.Disabled;
      --  Lock Region 2 registers.
      LOCKREG2      : LOCK_LOCKREG2_Field := NXP_SVD.PRINCE.Disabled;
      --  unspecified
      Reserved_3_7  : HAL.UInt5 := 16#0#;
      --  Lock the Mask registers.
      LOCKMASK      : LOCK_LOCKMASK_Field := NXP_SVD.PRINCE.Disabled;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOCK_Register use record
      LOCKREG0      at 0 range 0 .. 0;
      LOCKREG1      at 0 range 1 .. 1;
      LOCKREG2      at 0 range 2 .. 2;
      Reserved_3_7  at 0 range 3 .. 7;
      LOCKMASK      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype BASE_ADDR_ADDR_FIXED_Field is HAL.UInt18;
   subtype BASE_ADDR_ADDR_PRG_Field is HAL.UInt2;

   --  Base Address for region 0 register
   type BASE_ADDR_Register is record
      --  Read-only. Fixed portion of the base address of region 0.
      ADDR_FIXED     : BASE_ADDR_ADDR_FIXED_Field := 16#0#;
      --  Programmable portion of the base address of region 0.
      ADDR_PRG       : BASE_ADDR_ADDR_PRG_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BASE_ADDR_Register use record
      ADDR_FIXED     at 0 range 0 .. 17;
      ADDR_PRG       at 0 range 18 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  PRINCE
   type PRINCE_Peripheral is record
      --  Encryption Enable register
      ENC_ENABLE : aliased ENC_ENABLE_Register;
      --  Data Mask register, 32 Least Significant Bits
      MASK_LSB   : aliased HAL.UInt32;
      --  Data Mask register, 32 Most Significant Bits
      MASK_MSB   : aliased HAL.UInt32;
      --  Lock register
      LOCK       : aliased LOCK_Register;
      --  Initial Vector register for region 0, Least Significant Bits
      IV_LSB0    : aliased HAL.UInt32;
      --  Initial Vector register for region 0, Most Significant Bits
      IV_MSB0    : aliased HAL.UInt32;
      --  Base Address for region 0 register
      BASE_ADDR0 : aliased BASE_ADDR_Register;
      --  Sub-Region Enable register for region 0
      SR_ENABLE0 : aliased HAL.UInt32;
      --  Initial Vector register for region 1, Least Significant Bits
      IV_LSB1    : aliased HAL.UInt32;
      --  Initial Vector register for region 1, Most Significant Bits
      IV_MSB1    : aliased HAL.UInt32;
      --  Base Address for region 1 register
      BASE_ADDR1 : aliased BASE_ADDR_Register;
      --  Sub-Region Enable register for region 1
      SR_ENABLE1 : aliased HAL.UInt32;
      --  Initial Vector register for region 2, Least Significant Bits
      IV_LSB2    : aliased HAL.UInt32;
      --  Initial Vector register for region 2, Most Significant Bits
      IV_MSB2    : aliased HAL.UInt32;
      --  Base Address for region 2 register
      BASE_ADDR2 : aliased BASE_ADDR_Register;
      --  Sub-Region Enable register for region 2
      SR_ENABLE2 : aliased HAL.UInt32;
   end record
     with Volatile;

   for PRINCE_Peripheral use record
      ENC_ENABLE at 16#0# range 0 .. 31;
      MASK_LSB   at 16#4# range 0 .. 31;
      MASK_MSB   at 16#8# range 0 .. 31;
      LOCK       at 16#C# range 0 .. 31;
      IV_LSB0    at 16#10# range 0 .. 31;
      IV_MSB0    at 16#14# range 0 .. 31;
      BASE_ADDR0 at 16#18# range 0 .. 31;
      SR_ENABLE0 at 16#1C# range 0 .. 31;
      IV_LSB1    at 16#20# range 0 .. 31;
      IV_MSB1    at 16#24# range 0 .. 31;
      BASE_ADDR1 at 16#28# range 0 .. 31;
      SR_ENABLE1 at 16#2C# range 0 .. 31;
      IV_LSB2    at 16#30# range 0 .. 31;
      IV_MSB2    at 16#34# range 0 .. 31;
      BASE_ADDR2 at 16#38# range 0 .. 31;
      SR_ENABLE2 at 16#3C# range 0 .. 31;
   end record;

   --  PRINCE
   PRINCE_Periph : aliased PRINCE_Peripheral
     with Import, Address => System'To_Address (16#40035000#);

end NXP_SVD.PRINCE;
