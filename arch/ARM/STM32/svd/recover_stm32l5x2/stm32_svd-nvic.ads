--  This spec has been automatically generated from xyzzy

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.NVIC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  

   type ISER_Registers is array (0 .. 3) of HAL.UInt32;

   --  

   type ICER_Registers is array (0 .. 3) of HAL.UInt32;

   --  

   type ISPR_Registers is array (0 .. 3) of HAL.UInt32;

   --  

   type ICPR_Registers is array (0 .. 3) of HAL.UInt32;

   --  

   type IABR_Registers is array (0 .. 3) of HAL.UInt32;

   --  

   type ITNS_Registers is array (0 .. 15) of HAL.UInt32;

   --  IPR_IPR_N array element
   subtype IPR_IPR_N_Element is HAL.UInt8;

   --  IPR_IPR_N array
   type IPR_IPR_N_Field_Array is array (0 .. 3) of IPR_IPR_N_Element
     with Component_Size => 8, Size => 32;

   type IPR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IPR_N as a value
            Val : HAL.UInt32;
         when True =>
            --  IPR_N as an array
            Arr : IPR_IPR_N_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IPR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   type IPR_Registers is array (0 .. 29) of IPR_Register;

   subtype STIR_INTID_Field is HAL.UInt9;

   --  Software trigger interrupt register
   type STIR_Register is record
      --  Software generated interrupt ID
      INTID         : STIR_INTID_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STIR_Register use record
      INTID         at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Nested Vectored Interrupt Controller
   type NVIC_Peripheral is record
      ISER : aliased ISER_Registers;
      ICER : aliased ICER_Registers;
      ISPR : aliased ISPR_Registers;
      ICPR : aliased ICPR_Registers;
      IABR : aliased IABR_Registers;
      ITNS : aliased ITNS_Registers;
      IPR  : aliased IPR_Registers;
   end record
     with Volatile;

   for NVIC_Peripheral use record
      ISER at 16#0# range 0 .. 127;
      ICER at 16#80# range 0 .. 127;
      ISPR at 16#100# range 0 .. 127;
      ICPR at 16#180# range 0 .. 127;
      IABR at 16#200# range 0 .. 127;
      ITNS at 16#280# range 0 .. 511;
      IPR  at 16#300# range 0 .. 959;
   end record;

   --  Nested Vectored Interrupt Controller
   NVIC_Periph : aliased NVIC_Peripheral
     with Import, Address => NVIC_Base;

   --  Nested vectored interrupt controller
   type NVIC_STIR_Peripheral is record
      --  Software trigger interrupt register
      STIR : aliased STIR_Register;
   end record
     with Volatile;

   for NVIC_STIR_Peripheral use record
      STIR at 0 range 0 .. 31;
   end record;

   --  Nested vectored interrupt controller
   NVIC_STIR_Periph : aliased NVIC_STIR_Peripheral
     with Import, Address => NVIC_STIR_Base;

end STM32_SVD.NVIC;
