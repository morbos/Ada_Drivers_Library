--  This spec has been automatically generated from xyzzy

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.HASH is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_DATATYPE_Field is HAL.UInt2;
   subtype CR_NBW_Field is HAL.UInt4;

   type CR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      INIT           : Boolean := False;
      DMAE           : Boolean := False;
      DATATYPE       : CR_DATATYPE_Field := 16#0#;
      MODE           : Boolean := False;
      ALGO           : Boolean := False;
      NBW            : CR_NBW_Field := 16#0#;
      DINNE          : Boolean := False;
      MDMAT          : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      LKEY           : Boolean := False;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      ALGO_1         : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      INIT           at 0 range 2 .. 2;
      DMAE           at 0 range 3 .. 3;
      DATATYPE       at 0 range 4 .. 5;
      MODE           at 0 range 6 .. 6;
      ALGO           at 0 range 7 .. 7;
      NBW            at 0 range 8 .. 11;
      DINNE          at 0 range 12 .. 12;
      MDMAT          at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      LKEY           at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      ALGO_1         at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype STR_NBLW_Field is HAL.UInt5;

   type STR_Register is record
      NBLW          : STR_NBLW_Field := 16#0#;
      --  unspecified
      Reserved_5_7  : HAL.UInt3 := 16#0#;
      DCAL          : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STR_Register use record
      NBLW          at 0 range 0 .. 4;
      Reserved_5_7  at 0 range 5 .. 7;
      DCAL          at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  

   type HRA_Registers is array (0 .. 4) of HAL.UInt32;

   type IMR_Register is record
      DINIE         : Boolean := False;
      DCIE          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR_Register use record
      DINIE         at 0 range 0 .. 0;
      DCIE          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   type SR_Register is record
      DINIS         : Boolean := False;
      DCIS          : Boolean := False;
      DMAS          : Boolean := False;
      BUSY          : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      DINIS         at 0 range 0 .. 0;
      DCIS          at 0 range 1 .. 1;
      DMAS          at 0 range 2 .. 2;
      BUSY          at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  

   type CSR_Registers is array (0 .. 53) of HAL.UInt32;

   --  

   type HR_Registers is array (0 .. 7) of HAL.UInt32;

   -----------------
   -- Peripherals --
   -----------------

   type HASH_Peripheral is record
      CR  : aliased CR_Register;
      DIN : aliased HAL.UInt32;
      STR : aliased STR_Register;
      HRA : aliased HRA_Registers;
      IMR : aliased IMR_Register;
      SR  : aliased SR_Register;
      CSR : aliased CSR_Registers;
      HR  : aliased HR_Registers;
   end record
     with Volatile;

   for HASH_Peripheral use record
      CR  at 16#0# range 0 .. 31;
      DIN at 16#4# range 0 .. 31;
      STR at 16#8# range 0 .. 31;
      HRA at 16#C# range 0 .. 159;
      IMR at 16#20# range 0 .. 31;
      SR  at 16#24# range 0 .. 31;
      CSR at 16#F8# range 0 .. 1727;
      HR  at 16#310# range 0 .. 255;
   end record;

   HASH_Periph : aliased HASH_Peripheral
     with Import, Address => HASH_Base;

   SEC_HASH_Periph : aliased HASH_Peripheral
     with Import, Address => SEC_HASH_Base;

end STM32_SVD.HASH;
