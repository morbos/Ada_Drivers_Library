with STM32.Setup;

package body STM32.Board is

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
      LEDs : GPIO_Points := Red_LED & Green_LED; --  & Blue_LED;
   begin
      Enable_Clock (LEDs);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_2MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (LEDs,
                    Config => Configuration);
   end Initialize_LEDs;

   procedure All_LEDs_Off is
   begin
      --      Turn_Off (LCH_LED);
      null;
   end All_LEDs_Off;

   procedure Initialize_Board is
   begin
      Initialize_LEDs;
   end Initialize_Board;

end STM32.Board;
