--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.WWDG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype WWDG_CR_T_Field is HAL.UInt7;

   --  WWDG control register
   type WWDG_CR_Register is record
      --  7-bit counter (MSB to LSB) These bits contain the value of the
      --  watchdog counter, decremented every (4096 x 2<sup>WDGTB[2:0]</sup>)
      --  PCLK cycles. A reset is produced when it is decremented from 0x40 to
      --  0x3F (T6 becomes cleared).
      T             : WWDG_CR_T_Field := 16#7F#;
      --  Activation bit This bit is set by software and only cleared by
      --  hardware after a reset. When WDGA=1, the watchdog can generate a
      --  reset.
      WDGA          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WWDG_CR_Register use record
      T             at 0 range 0 .. 6;
      WDGA          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype WWDG_CFR_W_Field is HAL.UInt7;
   subtype WWDG_CFR_WDGTB_Field is HAL.UInt3;

   --  WWDG configuration register
   type WWDG_CFR_Register is record
      --  7-bit window value These bits contain the window value to be compared
      --  with the down-counter.
      W              : WWDG_CFR_W_Field := 16#7F#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  Early wake-up interrupt enable Set by software and cleared by
      --  hardware after a reset. When set, an interrupt occurs whenever the
      --  counter reaches the value 0x40.
      EWI            : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  Timer base The timebase of the prescaler can be modified as follows:
      WDGTB          : WWDG_CFR_WDGTB_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WWDG_CFR_Register use record
      W              at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      EWI            at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      WDGTB          at 0 range 11 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  WWDG status register
   type WWDG_SR_Register is record
      --  Early wake-up interrupt flag This bit is set by hardware when the
      --  counter has reached the value 0x40. It must be cleared by software by
      --  writing 0. Writing 1 has no effect. This bit is also set if the
      --  interrupt is not enabled.
      EWIF          : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WWDG_SR_Register use record
      EWIF          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  WWDG address block description
   type WWDG_Peripheral is record
      --  WWDG control register
      WWDG_CR  : aliased WWDG_CR_Register;
      --  WWDG configuration register
      WWDG_CFR : aliased WWDG_CFR_Register;
      --  WWDG status register
      WWDG_SR  : aliased WWDG_SR_Register;
   end record
     with Volatile;

   for WWDG_Peripheral use record
      WWDG_CR  at 16#0# range 0 .. 31;
      WWDG_CFR at 16#4# range 0 .. 31;
      WWDG_SR  at 16#8# range 0 .. 31;
   end record;

   --  WWDG address block description
   WWDG_Periph : aliased WWDG_Peripheral
     with Import, Address => WWDG_Base;

end STM32_SVD.WWDG;
