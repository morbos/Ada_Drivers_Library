with STM32.Setup;
with STM32_SVD.RCC;     use STM32_SVD.RCC;
with STM32_SVD.DBGMCU;  use STM32_SVD.DBGMCU;

package body STM32.Board is
   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (LED1);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_2MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (LED1,
                    Config => Configuration);
   end Initialize_LEDs;

   procedure Initialize_Board is
   begin

      --  Two steps here to enable the debugger in WFI
      --  1) Enable the DBGMCU regs.
      RCC_Periph.APB2ENR.DBGEN := True;
      --  2) Allow debug in these 3 states
      --  w/o this setting, a WFI is goodbye to your gdb session.
      DBGMCU_Periph.CR :=
        (DBG_SLEEP   => True,
         DBG_STOP    => True,
         DBG_STANDBY => True,
         others      => <>);

      Initialize_LEDs;

   end Initialize_Board;

end STM32.Board;
