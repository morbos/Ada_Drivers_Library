with STM32.Setup;

package body STM32.Board is

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (Green_LED);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_2MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (Green_LED,
                    Config => Configuration);
   end Initialize_LEDs;

   procedure All_LEDs_Off is
   begin
      Turn_Off (LCH_LED);
   end All_LEDs_Off;

   procedure Initialize_Board is
   begin
--      Configure_AF_SWD;
      --  ^^^ At PoR JTAG+SWD are ena. Reclaim JTAG pins

      Initialize_LEDs;

   end Initialize_Board;

end STM32.Board;
