--  This spec has been automatically generated from STM32L151.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.EXTI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype IMR_MR_Field is HAL.UInt23;

   --  IMR
   type IMR_Register is record
      --  Interrupt mask on line x
      MR             : IMR_MR_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR_Register use record
      MR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype EMR_MR_Field is HAL.UInt23;

   --  EMR
   type EMR_Register is record
      --  Event mask on line x
      MR             : EMR_MR_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EMR_Register use record
      MR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype RTSR_TR_Field is HAL.UInt23;

   --  RTSR
   type RTSR_Register is record
      --  Rising edge trigger event configuration bit of line x
      TR             : RTSR_TR_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR_Register use record
      TR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype FTSR_TR_Field is HAL.UInt23;

   --  FTSR
   type FTSR_Register is record
      --  Falling edge trigger event configuration bit of line x
      TR             : FTSR_TR_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR_Register use record
      TR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype SWIER_SWIER_Field is HAL.UInt23;

   --  SWIER
   type SWIER_Register is record
      --  Software interrupt on line x
      SWIER          : SWIER_SWIER_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER_Register use record
      SWIER          at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype PR_PR_Field is HAL.UInt23;

   --  PR
   type PR_Register is record
      --  Pending bit
      PR             : PR_PR_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PR_Register use record
      PR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  External interrupt/event controller
   type EXTI_Peripheral is record
      --  IMR
      IMR   : aliased IMR_Register;
      --  EMR
      EMR   : aliased EMR_Register;
      --  RTSR
      RTSR  : aliased RTSR_Register;
      --  FTSR
      FTSR  : aliased FTSR_Register;
      --  SWIER
      SWIER : aliased SWIER_Register;
      --  PR
      PR    : aliased PR_Register;
   end record
     with Volatile;

   for EXTI_Peripheral use record
      IMR   at 16#0# range 0 .. 31;
      EMR   at 16#4# range 0 .. 31;
      RTSR  at 16#8# range 0 .. 31;
      FTSR  at 16#C# range 0 .. 31;
      SWIER at 16#10# range 0 .. 31;
      PR    at 16#14# range 0 .. 31;
   end record;

   --  External interrupt/event controller
   EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => System'To_Address (16#40010400#);

end STM32_SVD.EXTI;
