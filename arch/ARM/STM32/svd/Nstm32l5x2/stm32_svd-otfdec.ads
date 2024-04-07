--  This spec has been automatically generated from STM32L562.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.OTFDEC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  OTFDEC control register
   type CR_Register is record
      --  Encryption mode bit
      ENC           : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      ENC           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype R1CFGR_MODE_Field is HAL.UInt2;
   subtype R1CFGR_KEYCRC_Field is HAL.UInt8;
   subtype R1CFGR_REGx_VERSION_Field is HAL.UInt16;

   --  OTFDEC region x configuration register
   type R1CFGR_Register is record
      --  region on-the-fly decryption enable
      REG_EN       : Boolean := False;
      --  region config lock
      CONFIGLOCK   : Boolean := False;
      --  region key lock
      KEYLOCK      : Boolean := False;
      --  unspecified
      Reserved_3_3 : HAL.Bit := 16#0#;
      --  operating mode
      MODE         : R1CFGR_MODE_Field := 16#0#;
      --  unspecified
      Reserved_6_7 : HAL.UInt2 := 16#0#;
      --  Read-only. region key 8-bit CRC
      KEYCRC       : R1CFGR_KEYCRC_Field := 16#0#;
      --  region firmware version
      REGx_VERSION : R1CFGR_REGx_VERSION_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for R1CFGR_Register use record
      REG_EN       at 0 range 0 .. 0;
      CONFIGLOCK   at 0 range 1 .. 1;
      KEYLOCK      at 0 range 2 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      MODE         at 0 range 4 .. 5;
      Reserved_6_7 at 0 range 6 .. 7;
      KEYCRC       at 0 range 8 .. 15;
      REGx_VERSION at 0 range 16 .. 31;
   end record;

   subtype R2CFGR_MODE_Field is HAL.UInt2;
   subtype R2CFGR_KEYCRC_Field is HAL.UInt8;
   subtype R2CFGR_REGx_VERSION_Field is HAL.UInt16;

   --  OTFDEC region x configuration register
   type R2CFGR_Register is record
      --  region on-the-fly decryption enable
      REG_EN       : Boolean := False;
      --  region config lock
      CONFIGLOCK   : Boolean := False;
      --  region key lock
      KEYLOCK      : Boolean := False;
      --  unspecified
      Reserved_3_3 : HAL.Bit := 16#0#;
      --  operating mode
      MODE         : R2CFGR_MODE_Field := 16#0#;
      --  unspecified
      Reserved_6_7 : HAL.UInt2 := 16#0#;
      --  Read-only. region key 8-bit CRC
      KEYCRC       : R2CFGR_KEYCRC_Field := 16#0#;
      --  region firmware version
      REGx_VERSION : R2CFGR_REGx_VERSION_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for R2CFGR_Register use record
      REG_EN       at 0 range 0 .. 0;
      CONFIGLOCK   at 0 range 1 .. 1;
      KEYLOCK      at 0 range 2 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      MODE         at 0 range 4 .. 5;
      Reserved_6_7 at 0 range 6 .. 7;
      KEYCRC       at 0 range 8 .. 15;
      REGx_VERSION at 0 range 16 .. 31;
   end record;

   subtype R3CFGR_MODE_Field is HAL.UInt2;
   subtype R3CFGR_KEYCRC_Field is HAL.UInt8;
   subtype R3CFGR_REGx_VERSION_Field is HAL.UInt16;

   --  OTFDEC region x configuration register
   type R3CFGR_Register is record
      --  region on-the-fly decryption enable
      REG_EN       : Boolean := False;
      --  region config lock
      CONFIGLOCK   : Boolean := False;
      --  region key lock
      KEYLOCK      : Boolean := False;
      --  unspecified
      Reserved_3_3 : HAL.Bit := 16#0#;
      --  operating mode
      MODE         : R3CFGR_MODE_Field := 16#0#;
      --  unspecified
      Reserved_6_7 : HAL.UInt2 := 16#0#;
      --  Read-only. region key 8-bit CRC
      KEYCRC       : R3CFGR_KEYCRC_Field := 16#0#;
      --  region firmware version
      REGx_VERSION : R3CFGR_REGx_VERSION_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for R3CFGR_Register use record
      REG_EN       at 0 range 0 .. 0;
      CONFIGLOCK   at 0 range 1 .. 1;
      KEYLOCK      at 0 range 2 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      MODE         at 0 range 4 .. 5;
      Reserved_6_7 at 0 range 6 .. 7;
      KEYCRC       at 0 range 8 .. 15;
      REGx_VERSION at 0 range 16 .. 31;
   end record;

   subtype R4CFGR_MODE_Field is HAL.UInt2;
   subtype R4CFGR_KEYCRC_Field is HAL.UInt8;
   subtype R4CFGR_REGx_VERSION_Field is HAL.UInt16;

   --  OTFDEC region x configuration register
   type R4CFGR_Register is record
      --  region on-the-fly decryption enable
      REG_EN       : Boolean := False;
      --  region config lock
      CONFIGLOCK   : Boolean := False;
      --  region key lock
      KEYLOCK      : Boolean := False;
      --  unspecified
      Reserved_3_3 : HAL.Bit := 16#0#;
      --  operating mode
      MODE         : R4CFGR_MODE_Field := 16#0#;
      --  unspecified
      Reserved_6_7 : HAL.UInt2 := 16#0#;
      --  Read-only. region key 8-bit CRC
      KEYCRC       : R4CFGR_KEYCRC_Field := 16#0#;
      --  region firmware version
      REGx_VERSION : R4CFGR_REGx_VERSION_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for R4CFGR_Register use record
      REG_EN       at 0 range 0 .. 0;
      CONFIGLOCK   at 0 range 1 .. 1;
      KEYLOCK      at 0 range 2 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      MODE         at 0 range 4 .. 5;
      Reserved_6_7 at 0 range 6 .. 7;
      KEYCRC       at 0 range 8 .. 15;
      REGx_VERSION at 0 range 16 .. 31;
   end record;

   --  OTFDEC interrupt status register
   type ISR_Register is record
      --  Read-only. Security Error Interrupt Flag status
      SEIF          : Boolean;
      --  Read-only. Execute-only execute-Never Error Interrupt Flag status
      XONEIF        : Boolean;
      --  Read-only. Key Error Interrupt Flag status
      KEIF          : Boolean;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      SEIF          at 0 range 0 .. 0;
      XONEIF        at 0 range 1 .. 1;
      KEIF          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  OTFDEC interrupt clear register
   type ICR_Register is record
      --  Write-only. SEIF
      SEIF          : Boolean := False;
      --  Write-only. Execute-only execute-Never Error Interrupt Flag clear
      XONEIF        : Boolean := False;
      --  Write-only. KEIF
      KEIF          : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      SEIF          at 0 range 0 .. 0;
      XONEIF        at 0 range 1 .. 1;
      KEIF          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  OTFDEC interrupt enable register
   type IER_Register is record
      --  Security Error Interrupt Enable
      SEIE          : Boolean := False;
      --  XONEIE
      XONEIE        : Boolean := False;
      --  KEIE
      KEIE          : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      SEIE          at 0 range 0 .. 0;
      XONEIE        at 0 range 1 .. 1;
      KEIE          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  On-The-Fly Decryption engine
   type OTFDEC_Peripheral is record
      --  OTFDEC control register
      CR          : aliased CR_Register;
      --  OTFDEC region x configuration register
      R1CFGR      : aliased R1CFGR_Register;
      --  OTFDEC region x start address register
      R1STARTADDR : aliased HAL.UInt32;
      --  OTFDEC region x end address register
      R1ENDADDR   : aliased HAL.UInt32;
      --  OTFDEC region x nonce register 0
      R1NONCER0   : aliased HAL.UInt32;
      --  OTFDEC region x nonce register 1
      R1NONCER1   : aliased HAL.UInt32;
      --  OTFDEC region x key register 0
      R1KEYR0     : aliased HAL.UInt32;
      --  OTFDEC region x key register 1
      R1KEYR1     : aliased HAL.UInt32;
      --  OTFDEC region x key register 2
      R1KEYR2     : aliased HAL.UInt32;
      --  OTFDEC region x key register 3
      R1KEYR3     : aliased HAL.UInt32;
      --  OTFDEC region x configuration register
      R2CFGR      : aliased R2CFGR_Register;
      --  OTFDEC region x start address register
      R2STARTADDR : aliased HAL.UInt32;
      --  OTFDEC region x end address register
      R2ENDADDR   : aliased HAL.UInt32;
      --  OTFDEC region x nonce register 0
      R2NONCER0   : aliased HAL.UInt32;
      --  OTFDEC region x nonce register 1
      R2NONCER1   : aliased HAL.UInt32;
      --  OTFDEC region x key register 0
      R2KEYR0     : aliased HAL.UInt32;
      --  OTFDEC region x key register 1
      R2KEYR1     : aliased HAL.UInt32;
      --  OTFDEC region x key register 2
      R2KEYR2     : aliased HAL.UInt32;
      --  OTFDEC region x key register 3
      R2KEYR3     : aliased HAL.UInt32;
      --  OTFDEC region x configuration register
      R3CFGR      : aliased R3CFGR_Register;
      --  OTFDEC region x start address register
      R3STARTADDR : aliased HAL.UInt32;
      --  OTFDEC region x end address register
      R3ENDADDR   : aliased HAL.UInt32;
      --  OTFDEC region x nonce register 0
      R3NONCER0   : aliased HAL.UInt32;
      --  OTFDEC region x nonce register 1
      R3NONCER1   : aliased HAL.UInt32;
      --  OTFDEC region x key register 0
      R3KEYR0     : aliased HAL.UInt32;
      --  OTFDEC region x key register 1
      R3KEYR1     : aliased HAL.UInt32;
      --  OTFDEC region x key register 2
      R3KEYR2     : aliased HAL.UInt32;
      --  OTFDEC region x key register 3
      R3KEYR3     : aliased HAL.UInt32;
      --  OTFDEC region x configuration register
      R4CFGR      : aliased R4CFGR_Register;
      --  OTFDEC region x start address register
      R4STARTADDR : aliased HAL.UInt32;
      --  OTFDEC region x end address register
      R4ENDADDR   : aliased HAL.UInt32;
      --  OTFDEC region x nonce register 0
      R4NONCER0   : aliased HAL.UInt32;
      --  OTFDEC region x nonce register 1
      R4NONCER1   : aliased HAL.UInt32;
      --  OTFDEC region x key register 0
      R4KEYR0     : aliased HAL.UInt32;
      --  OTFDEC region x key register 1
      R4KEYR1     : aliased HAL.UInt32;
      --  OTFDEC region x key register 2
      R4KEYR2     : aliased HAL.UInt32;
      --  OTFDEC region x key register 3
      R4KEYR3     : aliased HAL.UInt32;
      --  OTFDEC interrupt status register
      ISR         : aliased ISR_Register;
      --  OTFDEC interrupt clear register
      ICR         : aliased ICR_Register;
      --  OTFDEC interrupt enable register
      IER         : aliased IER_Register;
   end record
     with Volatile;

   for OTFDEC_Peripheral use record
      CR          at 16#0# range 0 .. 31;
      R1CFGR      at 16#20# range 0 .. 31;
      R1STARTADDR at 16#24# range 0 .. 31;
      R1ENDADDR   at 16#28# range 0 .. 31;
      R1NONCER0   at 16#2C# range 0 .. 31;
      R1NONCER1   at 16#30# range 0 .. 31;
      R1KEYR0     at 16#34# range 0 .. 31;
      R1KEYR1     at 16#38# range 0 .. 31;
      R1KEYR2     at 16#3C# range 0 .. 31;
      R1KEYR3     at 16#40# range 0 .. 31;
      R2CFGR      at 16#50# range 0 .. 31;
      R2STARTADDR at 16#54# range 0 .. 31;
      R2ENDADDR   at 16#58# range 0 .. 31;
      R2NONCER0   at 16#5C# range 0 .. 31;
      R2NONCER1   at 16#60# range 0 .. 31;
      R2KEYR0     at 16#64# range 0 .. 31;
      R2KEYR1     at 16#68# range 0 .. 31;
      R2KEYR2     at 16#6C# range 0 .. 31;
      R2KEYR3     at 16#70# range 0 .. 31;
      R3CFGR      at 16#80# range 0 .. 31;
      R3STARTADDR at 16#84# range 0 .. 31;
      R3ENDADDR   at 16#88# range 0 .. 31;
      R3NONCER0   at 16#8C# range 0 .. 31;
      R3NONCER1   at 16#90# range 0 .. 31;
      R3KEYR0     at 16#94# range 0 .. 31;
      R3KEYR1     at 16#98# range 0 .. 31;
      R3KEYR2     at 16#9C# range 0 .. 31;
      R3KEYR3     at 16#A0# range 0 .. 31;
      R4CFGR      at 16#B0# range 0 .. 31;
      R4STARTADDR at 16#B4# range 0 .. 31;
      R4ENDADDR   at 16#B8# range 0 .. 31;
      R4NONCER0   at 16#BC# range 0 .. 31;
      R4NONCER1   at 16#C0# range 0 .. 31;
      R4KEYR0     at 16#C4# range 0 .. 31;
      R4KEYR1     at 16#C8# range 0 .. 31;
      R4KEYR2     at 16#CC# range 0 .. 31;
      R4KEYR3     at 16#D0# range 0 .. 31;
      ISR         at 16#300# range 0 .. 31;
      ICR         at 16#304# range 0 .. 31;
      IER         at 16#308# range 0 .. 31;
   end record;

   --  On-The-Fly Decryption engine
   OTFDEC1_Periph : aliased OTFDEC_Peripheral
     with Import, Address => System'To_Address (16#420C5000#);

   --  On-The-Fly Decryption engine
   SEC_OTFDEC1_Periph : aliased OTFDEC_Peripheral
     with Import, Address => System'To_Address (16#520C5000#);

end STM32_SVD.OTFDEC;
