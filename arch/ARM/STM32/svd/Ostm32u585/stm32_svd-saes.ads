--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SAES is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_DATATYPE_Field is HAL.UInt2;
   subtype CR_MODE_Field is HAL.UInt2;
   subtype CR_CHMOD_Field is HAL.UInt2;
   subtype CR_KMOD_Field is HAL.UInt2;
   subtype CR_KSHAREID_Field is HAL.UInt2;
   subtype CR_KEYSEL_Field is HAL.UInt3;

   --  control register
   type CR_Register is record
      --  SAES enable
      EN             : Boolean := False;
      --  DATATYPE
      DATATYPE       : CR_DATATYPE_Field := 16#0#;
      --  MODE
      MODE           : CR_MODE_Field := 16#0#;
      --  CHMOD
      CHMOD          : CR_CHMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_10  : HAL.UInt4 := 16#0#;
      --  DMAINEN
      DMAINEN        : Boolean := False;
      --  DMAOUTEN
      DMAOUTEN       : Boolean := False;
      --  unspecified
      Reserved_13_17 : HAL.UInt5 := 16#0#;
      --  KEYSIZE
      KEYSIZE        : Boolean := False;
      --  KEYPROT
      KEYPROT        : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  KMOD
      KMOD           : CR_KMOD_Field := 16#0#;
      --  KSHAREID
      KSHAREID       : CR_KSHAREID_Field := 16#0#;
      --  KEYSEL
      KEYSEL         : CR_KEYSEL_Field := 16#0#;
      --  IPRST
      IPRST          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      EN             at 0 range 0 .. 0;
      DATATYPE       at 0 range 1 .. 2;
      MODE           at 0 range 3 .. 4;
      CHMOD          at 0 range 5 .. 6;
      Reserved_7_10  at 0 range 7 .. 10;
      DMAINEN        at 0 range 11 .. 11;
      DMAOUTEN       at 0 range 12 .. 12;
      Reserved_13_17 at 0 range 13 .. 17;
      KEYSIZE        at 0 range 18 .. 18;
      KEYPROT        at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      KMOD           at 0 range 24 .. 25;
      KSHAREID       at 0 range 26 .. 27;
      KEYSEL         at 0 range 28 .. 30;
      IPRST          at 0 range 31 .. 31;
   end record;

   --  status register
   type SR_Register is record
      --  Read-only. Computation complete flag
      CCF           : Boolean;
      --  Read-only. Read error flag
      RDERR         : Boolean;
      --  Read-only. Write error flag
      WRERR         : Boolean;
      --  Read-only. BUSY
      BUSY          : Boolean;
      --  unspecified
      Reserved_4_6  : HAL.UInt3;
      --  Read-only. Key Valid flag
      KEYVALID      : Boolean;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      CCF           at 0 range 0 .. 0;
      RDERR         at 0 range 1 .. 1;
      WRERR         at 0 range 2 .. 2;
      BUSY          at 0 range 3 .. 3;
      Reserved_4_6  at 0 range 4 .. 6;
      KEYVALID      at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DPACFGR_TRIMCFG_Field is HAL.UInt2;

   --  configuration register
   type DPACFGR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  REDCFG
      REDCFG        : Boolean := False;
      --  RESEED
      RESEED        : Boolean := False;
      --  TRIMCFG
      TRIMCFG       : DPACFGR_TRIMCFG_Field := 16#1#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  Read-only. CONFIGLOCK
      CONFIGLOCK    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DPACFGR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      REDCFG        at 0 range 1 .. 1;
      RESEED        at 0 range 2 .. 2;
      TRIMCFG       at 0 range 3 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONFIGLOCK    at 0 range 31 .. 31;
   end record;

   --  interrupt enable register
   type IER_Register is record
      --  Computation complete flag interrupt enable
      CCFIE         : Boolean := False;
      --  Read or write error interrupt enable
      RWEIE         : Boolean := False;
      --  Key error interrupt enable
      KEIE          : Boolean := False;
      --  RNGEIE
      RNGEIE        : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      CCFIE         at 0 range 0 .. 0;
      RWEIE         at 0 range 1 .. 1;
      KEIE          at 0 range 2 .. 2;
      RNGEIE        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  interrupt status register
   type ISR_Register is record
      --  Read-only. Computation complete flag
      CCF           : Boolean;
      --  Read-only. Read or write error interrupt flag
      RWEIF         : Boolean;
      --  Read-only. Key error interrupt flag
      KEIF          : Boolean;
      --  Read-only. RNGEIF
      RNGEIF        : Boolean;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      CCF           at 0 range 0 .. 0;
      RWEIF         at 0 range 1 .. 1;
      KEIF          at 0 range 2 .. 2;
      RNGEIF        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  interrupt clear register
   type ICR_Register is record
      --  Write-only. Computation complete flag clear
      CCF           : Boolean := False;
      --  Write-only. Read or write error interrupt flag clear
      RWEIF         : Boolean := False;
      --  Write-only. Key error interrupt flag clear
      KEIF          : Boolean := False;
      --  Write-only. RNGEIF
      RNGEIF        : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      CCF           at 0 range 0 .. 0;
      RWEIF         at 0 range 1 .. 1;
      KEIF          at 0 range 2 .. 2;
      RNGEIF        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Secure AES coprocessor
   type SAES_Peripheral is record
      --  control register
      CR      : aliased CR_Register;
      --  status register
      SR      : aliased SR_Register;
      --  data input register
      DINR    : aliased HAL.UInt32;
      --  data output register
      DOUTR   : aliased HAL.UInt32;
      --  key register 0
      KEYR0   : aliased HAL.UInt32;
      --  key register 1
      KEYR1   : aliased HAL.UInt32;
      --  key register 2
      KEYR2   : aliased HAL.UInt32;
      --  key register 3
      KEYR3   : aliased HAL.UInt32;
      --  initialization vector register 0
      IVR0    : aliased HAL.UInt32;
      --  initialization vector register 1
      IVR1    : aliased HAL.UInt32;
      --  initialization vector register 2
      IVR2    : aliased HAL.UInt32;
      --  initialization vector register 3
      IVR3    : aliased HAL.UInt32;
      --  key register 4
      KEYR4   : aliased HAL.UInt32;
      --  key register 5
      KEYR5   : aliased HAL.UInt32;
      --  key register 6
      KEYR6   : aliased HAL.UInt32;
      --  key register 7
      KEYR7   : aliased HAL.UInt32;
      --  configuration register
      DPACFGR : aliased DPACFGR_Register;
      --  interrupt enable register
      IER     : aliased IER_Register;
      --  interrupt status register
      ISR     : aliased ISR_Register;
      --  interrupt clear register
      ICR     : aliased ICR_Register;
   end record
     with Volatile;

   for SAES_Peripheral use record
      CR      at 16#0# range 0 .. 31;
      SR      at 16#4# range 0 .. 31;
      DINR    at 16#8# range 0 .. 31;
      DOUTR   at 16#C# range 0 .. 31;
      KEYR0   at 16#10# range 0 .. 31;
      KEYR1   at 16#14# range 0 .. 31;
      KEYR2   at 16#18# range 0 .. 31;
      KEYR3   at 16#1C# range 0 .. 31;
      IVR0    at 16#20# range 0 .. 31;
      IVR1    at 16#24# range 0 .. 31;
      IVR2    at 16#28# range 0 .. 31;
      IVR3    at 16#2C# range 0 .. 31;
      KEYR4   at 16#30# range 0 .. 31;
      KEYR5   at 16#34# range 0 .. 31;
      KEYR6   at 16#38# range 0 .. 31;
      KEYR7   at 16#3C# range 0 .. 31;
      DPACFGR at 16#100# range 0 .. 31;
      IER     at 16#300# range 0 .. 31;
      ISR     at 16#304# range 0 .. 31;
      ICR     at 16#308# range 0 .. 31;
   end record;

   --  Secure AES coprocessor
   SAES_Periph : aliased SAES_Peripheral
     with Import, Address => SAES_Base;

   --  Secure AES coprocessor
   SEC_SAES_Periph : aliased SAES_Peripheral
     with Import, Address => SEC_SAES_Base;

end STM32_SVD.SAES;
