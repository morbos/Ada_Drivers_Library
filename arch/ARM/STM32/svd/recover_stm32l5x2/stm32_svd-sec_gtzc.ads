--  This spec has been automatically generated from xyzzy

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SEC_GTZC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  MPCBB control register
   type MPCBB1_CR_Register is record
      --  LCK
      LCK           : Boolean := False;
      --  unspecified
      Reserved_1_29 : HAL.UInt29 := 16#0#;
      --  INVSECSTATE
      INVSECSTATE   : Boolean := False;
      --  SRWILADIS
      SRWILADIS     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_CR_Register use record
      LCK           at 0 range 0 .. 0;
      Reserved_1_29 at 0 range 1 .. 29;
      INVSECSTATE   at 0 range 30 .. 30;
      SRWILADIS     at 0 range 31 .. 31;
   end record;

   --  MPCBBx_LCKVTR_LCKSB array
   type MPCBBx_LCKVTR_LCKSB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   type MPCBBx_LCKVTR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LCKSB as a value
            Val : HAL.UInt32;
         when True =>
            --  LCKSB as an array
            Arr : MPCBBx_LCKVTR_LCKSB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBBx_LCKVTR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   type MPCBBx_LCKVTR_Registers is array (1 .. 2) of MPCBBx_LCKVTR_Register;

   --  MPCBBx_VCTR_B array
   type MPCBBx_VCTR_B_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   type MPCBBx_VCTR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBBx_VCTR_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBBx_VCTR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   type MPCBBx_VCTR_Registers is array (0 .. 63) of MPCBBx_VCTR_Register;

   --  MPCBB control register
   type MPCBB2_CR_Register is record
      --  LCK
      LCK           : Boolean := False;
      --  unspecified
      Reserved_1_29 : HAL.UInt29 := 16#0#;
      --  INVSECSTATE
      INVSECSTATE   : Boolean := False;
      --  SRWILADIS
      SRWILADIS     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_CR_Register use record
      LCK           at 0 range 0 .. 0;
      Reserved_1_29 at 0 range 1 .. 29;
      INVSECSTATE   at 0 range 30 .. 30;
      SRWILADIS     at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  SEC_GTZC_MPCBB1
   type SEC_GTZC_MPCBB1_Peripheral is record
      --  MPCBB control register
      MPCBB1_CR     : aliased MPCBB1_CR_Register;
      MPCBBx_LCKVTR : aliased MPCBBx_LCKVTR_Registers;
      MPCBBx_VCTR   : aliased MPCBBx_VCTR_Registers;
   end record
     with Volatile;

   for SEC_GTZC_MPCBB1_Peripheral use record
      MPCBB1_CR     at 16#0# range 0 .. 31;
      MPCBBx_LCKVTR at 16#10# range 0 .. 63;
      MPCBBx_VCTR   at 16#100# range 0 .. 2047;
   end record;

   --  SEC_GTZC_MPCBB1
   SEC_GTZC_MPCBB1_Periph : aliased SEC_GTZC_MPCBB1_Peripheral
     with Import, Address => SEC_GTZC_MPCBB1_Base;

   --  SEC_GTZC_MPCBB2
   type SEC_GTZC_MPCBB2_Peripheral is record
      --  MPCBB control register
      MPCBB2_CR     : aliased MPCBB2_CR_Register;
      MPCBBx_LCKVTR : aliased MPCBBx_LCKVTR_Registers;
      MPCBBx_VCTR   : aliased MPCBBx_VCTR_Registers;
   end record
     with Volatile;

   for SEC_GTZC_MPCBB2_Peripheral use record
      MPCBB2_CR     at 16#0# range 0 .. 31;
      MPCBBx_LCKVTR at 16#10# range 0 .. 63;
      MPCBBx_VCTR   at 16#100# range 0 .. 2047;
   end record;

   --  SEC_GTZC_MPCBB2
   SEC_GTZC_MPCBB2_Periph : aliased SEC_GTZC_MPCBB2_Peripheral
     with Import, Address => SEC_GTZC_MPCBB2_Base;

end STM32_SVD.SEC_GTZC;
