--  This spec has been automatically generated from xyzzy

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
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      ENC           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   ---------------------------
   -- R cluster's Registers --
   ---------------------------

   subtype CFGR_R_MODE_Field is HAL.UInt2;
   subtype CFGR_R_KEYCRC_Field is HAL.UInt8;
   subtype CFGR_R_REGx_VERSION_Field is HAL.UInt16;

   --  OTFDEC region x configuration register
   type CFGR_R_Register is record
      --  region on-the-fly decryption enable
      REG_EN       : Boolean := False;
      --  region config lock
      CONFIGLOCK   : Boolean := False;
      --  region key lock
      KEYLOCK      : Boolean := False;
      --  unspecified
      Reserved_3_3 : HAL.Bit := 16#0#;
      --  operating mode
      MODE         : CFGR_R_MODE_Field := 16#0#;
      --  unspecified
      Reserved_6_7 : HAL.UInt2 := 16#0#;
      --  Read-only. region key 8-bit CRC
      KEYCRC       : CFGR_R_KEYCRC_Field := 16#0#;
      --  region firmware version
      REGx_VERSION : CFGR_R_REGx_VERSION_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_R_Register use record
      REG_EN       at 0 range 0 .. 0;
      CONFIGLOCK   at 0 range 1 .. 1;
      KEYLOCK      at 0 range 2 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      MODE         at 0 range 4 .. 5;
      Reserved_6_7 at 0 range 6 .. 7;
      KEYCRC       at 0 range 8 .. 15;
      REGx_VERSION at 0 range 16 .. 31;
   end record;

   --  

   type NONCER_R_Registers is array (0 .. 1) of HAL.UInt32;

   --  

   type KEYR_R_Registers is array (0 .. 3) of HAL.UInt32;

   type R_Cluster is record
      --  OTFDEC region x configuration register
      CFGR      : aliased CFGR_R_Register;
      --  OTFDEC region x start address register
      STARTADDR : aliased HAL.UInt32;
      --  OTFDEC region x end address register
      ENDADDR   : aliased HAL.UInt32;
      NONCER    : aliased NONCER_R_Registers;
      KEYR      : aliased KEYR_R_Registers;
   end record
     with Size => 384;

   for R_Cluster use record
      CFGR      at 16#0# range 0 .. 31;
      STARTADDR at 16#4# range 0 .. 31;
      ENDADDR   at 16#8# range 0 .. 31;
      NONCER    at 16#C# range 0 .. 63;
      KEYR      at 16#14# range 0 .. 127;
   end record;

   type R_Clusters is array (1 .. 4) of R_Cluster;

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
     with Volatile_Full_Access, Object_Size => 32,
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
     with Volatile_Full_Access, Object_Size => 32,
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
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      SEIE          at 0 range 0 .. 0;
      XONEIE        at 0 range 1 .. 1;
      KEIE          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   ---------------------------
   -- R cluster's Registers --
   ---------------------------

   --  
   --  

   -----------------
   -- Peripherals --
   -----------------

   --  On-The-Fly Decryption engine
   type OTFDEC_Peripheral is record
      --  OTFDEC control register
      CR  : aliased CR_Register;
      R   : aliased R_Clusters;
      --  OTFDEC interrupt status register
      ISR : aliased ISR_Register;
      --  OTFDEC interrupt clear register
      ICR : aliased ICR_Register;
      --  OTFDEC interrupt enable register
      IER : aliased IER_Register;
   end record
     with Volatile;

   for OTFDEC_Peripheral use record
      CR  at 16#0# range 0 .. 31;
      R   at 16#20# range 0 .. 1535;
      ISR at 16#300# range 0 .. 31;
      ICR at 16#304# range 0 .. 31;
      IER at 16#308# range 0 .. 31;
   end record;

   --  On-The-Fly Decryption engine
   OTFDEC1_Periph : aliased OTFDEC_Peripheral
     with Import, Address => OTFDEC1_Base;

   --  On-The-Fly Decryption engine
   SEC_OTFDEC1_Periph : aliased OTFDEC_Peripheral
     with Import, Address => SEC_OTFDEC1_Base;

end STM32_SVD.OTFDEC;
