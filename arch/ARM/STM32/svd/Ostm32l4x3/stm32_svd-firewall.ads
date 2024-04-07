--  This spec has been automatically generated from STM32L4x3.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.Firewall is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CSSA_ADD_Field is HAL.UInt16;

   --  Code segment start address
   type CSSA_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  code segment start address
      ADD            : CSSA_ADD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSSA_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      ADD            at 0 range 8 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CSL_LENG_Field is HAL.UInt14;

   --  Code segment length
   type CSL_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  code segment length
      LENG           : CSL_LENG_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSL_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      LENG           at 0 range 8 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype NVDSSA_ADD_Field is HAL.UInt16;

   --  Non-volatile data segment start address
   type NVDSSA_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Non-volatile data segment start address
      ADD            : NVDSSA_ADD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for NVDSSA_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      ADD            at 0 range 8 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype NVDSL_LENG_Field is HAL.UInt14;

   --  Non-volatile data segment length
   type NVDSL_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Non-volatile data segment length
      LENG           : NVDSL_LENG_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for NVDSL_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      LENG           at 0 range 8 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype VDSSA_ADD_Field is HAL.UInt10;

   --  Volatile data segment start address
   type VDSSA_Register is record
      --  unspecified
      Reserved_0_5   : HAL.UInt6 := 16#0#;
      --  Volatile data segment start address
      ADD            : VDSSA_ADD_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for VDSSA_Register use record
      Reserved_0_5   at 0 range 0 .. 5;
      ADD            at 0 range 6 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype VDSL_LENG_Field is HAL.UInt10;

   --  Volatile data segment length
   type VDSL_Register is record
      --  unspecified
      Reserved_0_5   : HAL.UInt6 := 16#0#;
      --  Non-volatile data segment length
      LENG           : VDSL_LENG_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for VDSL_Register use record
      Reserved_0_5   at 0 range 0 .. 5;
      LENG           at 0 range 6 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Configuration register
   type CR_Register is record
      --  Firewall pre alarm
      FPA           : Boolean := False;
      --  Volatile data shared
      VDS           : Boolean := False;
      --  Volatile data execution
      VDE           : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      FPA           at 0 range 0 .. 0;
      VDS           at 0 range 1 .. 1;
      VDE           at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Firewall
   type FIREWALL_Peripheral is record
      --  Code segment start address
      CSSA   : aliased CSSA_Register;
      --  Code segment length
      CSL    : aliased CSL_Register;
      --  Non-volatile data segment start address
      NVDSSA : aliased NVDSSA_Register;
      --  Non-volatile data segment length
      NVDSL  : aliased NVDSL_Register;
      --  Volatile data segment start address
      VDSSA  : aliased VDSSA_Register;
      --  Volatile data segment length
      VDSL   : aliased VDSL_Register;
      --  Configuration register
      CR     : aliased CR_Register;
   end record
     with Volatile;

   for FIREWALL_Peripheral use record
      CSSA   at 16#0# range 0 .. 31;
      CSL    at 16#4# range 0 .. 31;
      NVDSSA at 16#8# range 0 .. 31;
      NVDSL  at 16#C# range 0 .. 31;
      VDSSA  at 16#10# range 0 .. 31;
      VDSL   at 16#14# range 0 .. 31;
      CR     at 16#20# range 0 .. 31;
   end record;

   --  Firewall
   FIREWALL_Periph : aliased FIREWALL_Peripheral
     with Import, Address => System'To_Address (16#40011C00#);

end STM32_SVD.Firewall;
