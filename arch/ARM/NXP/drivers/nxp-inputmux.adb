with HAL;               use HAL;
with NXP.Device;        use NXP.Device;
with NXP_SVD;           use NXP_SVD;
with NXP_SVD.SYSCON;    use NXP_SVD.SYSCON;
with NXP_SVD.INPUTMUX;  use NXP_SVD.INPUTMUX;
with System;            use System;
package body NXP.InputMux is

   procedure Enable_InputMux
   is
   begin
      SYSCON_Periph.AHBCLKCTRL0.MUX := Enable;
   end Enable_InputMux;
   procedure Disable_InputMux
   is
   begin
      SYSCON_Periph.AHBCLKCTRL0.MUX := Disable;
   end Disable_InputMux;

end NXP.InputMux;
