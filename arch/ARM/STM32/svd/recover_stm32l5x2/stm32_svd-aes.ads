--  This spec has been automatically generated from xyzzy

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.AES is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_DATATYPE_Field is HAL.UInt2;
   subtype CR_MODE_Field is HAL.UInt2;
   subtype CR_CHMOD_Field is HAL.UInt2;
   subtype CR_GCMPH_Field is HAL.UInt2;
   subtype CR_NPBLB_Field is HAL.UInt4;

   --  control register
   type CR_Register is record
      --  AES enable
      EN             : Boolean := False;
      --  Data type selection (for data in and data out to/from the
      --  cryptographic block)
      DATATYPE       : CR_DATATYPE_Field := 16#0#;
      --  AES operating mode
      MODE           : CR_MODE_Field := 16#0#;
      --  AES chaining mode selection Bit1 Bit0
      CHMOD          : CR_CHMOD_Field := 16#0#;
      --  Computation Complete Flag Clear
      CCFC           : Boolean := False;
      --  Error clear
      ERRC           : Boolean := False;
      --  CCF flag interrupt enable
      CCFIE          : Boolean := False;
      --  Error interrupt enable
      ERRIE          : Boolean := False;
      --  Enable DMA management of data input phase
      DMAINEN        : Boolean := False;
      --  Enable DMA management of data output phase
      DMAOUTEN       : Boolean := False;
      --  Used only for GCM, CCM and GMAC algorithms and has no effect when
      --  other algorithms are selected
      GCMPH          : CR_GCMPH_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  AES chaining mode Bit2
      CHMOD2         : Boolean := False;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  Key size selection
      KEYSIZE        : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Number of padding bytes in last block of payload
      NPBLB          : CR_NPBLB_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      EN             at 0 range 0 .. 0;
      DATATYPE       at 0 range 1 .. 2;
      MODE           at 0 range 3 .. 4;
      CHMOD          at 0 range 5 .. 6;
      CCFC           at 0 range 7 .. 7;
      ERRC           at 0 range 8 .. 8;
      CCFIE          at 0 range 9 .. 9;
      ERRIE          at 0 range 10 .. 10;
      DMAINEN        at 0 range 11 .. 11;
      DMAOUTEN       at 0 range 12 .. 12;
      GCMPH          at 0 range 13 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CHMOD2         at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      KEYSIZE        at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      NPBLB          at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  status register
   type SR_Register is record
      --  Read-only. Computation complete flag
      CCF           : Boolean;
      --  Read-only. Read error flag
      RDERR         : Boolean;
      --  Read-only. Write error flag
      WRERR         : Boolean;
      --  Read-only. Busy flag
      BUSY          : Boolean;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      CCF           at 0 range 0 .. 0;
      RDERR         at 0 range 1 .. 1;
      WRERR         at 0 range 2 .. 2;
      BUSY          at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  

   type KEYRA_Registers is array (0 .. 3) of HAL.UInt32;

   --  

   type IVR_Registers is array (0 .. 3) of HAL.UInt32;

   --  

   type KEYRB_Registers is array (4 .. 7) of HAL.UInt32;

   --  

   type SUSPR_Registers is array (0 .. 7) of HAL.UInt32;

   -----------------
   -- Peripherals --
   -----------------

   --  Advanced encryption standard hardware accelerator 1
   type AES_Peripheral is record
      --  control register
      CR    : aliased CR_Register;
      --  status register
      SR    : aliased SR_Register;
      --  data input register
      DINR  : aliased HAL.UInt32;
      --  data output register
      DOUTR : aliased HAL.UInt32;
      KEYRA : aliased KEYRA_Registers;
      IVR   : aliased IVR_Registers;
      KEYRB : aliased KEYRB_Registers;
      SUSPR : aliased SUSPR_Registers;
   end record
     with Volatile;

   for AES_Peripheral use record
      CR    at 16#0# range 0 .. 31;
      SR    at 16#4# range 0 .. 31;
      DINR  at 16#8# range 0 .. 31;
      DOUTR at 16#C# range 0 .. 31;
      KEYRA at 16#10# range 0 .. 127;
      IVR   at 16#20# range 0 .. 127;
      KEYRB at 16#30# range 0 .. 127;
      SUSPR at 16#40# range 0 .. 255;
   end record;

   --  Advanced encryption standard hardware accelerator 1
   AES_Periph : aliased AES_Peripheral
     with Import, Address => AES_Base;

   --  Advanced encryption standard hardware accelerator 1
   SEC_AES_Periph : aliased AES_Peripheral
     with Import, Address => SEC_AES_Base;

end STM32_SVD.AES;
