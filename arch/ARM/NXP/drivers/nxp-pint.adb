with HAL;               use HAL;
with NXP.Device;        use NXP.Device;
with NXP_SVD;           use NXP_SVD;
with NXP_SVD.SYSCON;    use NXP_SVD.SYSCON;
with NXP_SVD.PINT;      use NXP_SVD.PINT;
with System;            use System;

package body NXP.Pint is

   PINT : aliased PINT_Peripheral
     with Import, Address => S_NS_Periph (PINT_Base);
   procedure Enable_Pint
   is
   begin
      SYSCON_Periph.AHBCLKCTRL0.PINT := Enable;
   end Enable_Pint;

   procedure Disable_Pint
   is
   begin
      SYSCON_Periph.AHBCLKCTRL0.PINT := Disable;
   end Disable_Pint;

   function Pint_Interrupt_Pending (IntNum : Pint_PinInt) return Boolean
   is
      Mask : UInt8 := (2 ** IntNum'Enum_Rep);
   begin
      return (PINT.IST.PSTAT and Mask) = Mask;
   end Pint_Interrupt_Pending;

   procedure Pint_Clear_Interrupt (IntNum : Pint_PinInt)
   is
      Mask : UInt8 := (2 ** IntNum'Enum_Rep);
   begin
      PINT.IST.PSTAT := Mask;
   end Pint_Clear_Interrupt;

--   PINT_PinInterruptConfig(PINT, kPINT_PinInt2, kPINT_PinIntEnableFallEdge, pint_intr_callback);
--    /* Enable callbacks for PINT2 by Index */
--    PINT_EnableCallbackByIndex(PINT, kPINT_PinInt2);

end NXP.Pint;
