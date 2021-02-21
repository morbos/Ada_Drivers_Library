with STM32.Setup;

package body STM32.Board is

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
      LED : GPIO_Point := Red_LED;
   begin
      Enable_Clock (LED);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_2MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (LED,
                    Config => Configuration);
   end Initialize_LEDs;

   procedure All_LEDs_Off is
   begin
      null;
   end All_LEDs_Off;

   procedure Initialize_Board is
   begin
      Initialize_LEDs;
   end Initialize_Board;

end STM32.Board;
