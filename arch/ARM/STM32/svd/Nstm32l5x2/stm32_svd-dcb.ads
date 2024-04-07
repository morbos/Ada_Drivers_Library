--  This spec has been automatically generated from STM32L562.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DCB is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Debug Security Control and Status Register
   type DSCSR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  Current domain Secure
      CDS            : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSCSR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      CDS            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Debug Control Block
   type DCB_Peripheral is record
      --  Debug Security Control and Status Register
      DSCSR : aliased DSCSR_Register;
   end record
     with Volatile;

   for DCB_Peripheral use record
      DSCSR at 0 range 0 .. 31;
   end record;

   --  Debug Control Block
   DCB_Periph : aliased DCB_Peripheral
     with Import, Address => System'To_Address (16#E000EE08#);

end STM32_SVD.DCB;
