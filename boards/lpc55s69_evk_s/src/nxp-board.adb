package body NXP.Board is
   procedure Initialize_Board
   is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (LEDs);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_Low;
      Configuration.Resistors   := Floating;
      Configure_IO (LEDs,
                    Config => Configuration);

      for Point of LEDs loop
         Turn_Off (Point);
      end loop;

   end Initialize_Board;
end NXP.Board;
