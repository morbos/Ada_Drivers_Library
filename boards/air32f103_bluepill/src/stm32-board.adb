with System;
with STM32.Setup;

package body STM32.Board is
   function IsAir32 return Boolean
   is
      ChipID : UInt32;
      SCB_Base : constant System.Address :=
        System'To_Address (16#E000ED00#);
      for ChipID'Address use SCB_Base;
   begin
      if ChipID = 16#412fc230# then
         return True;
      else
         return False;
      end if;
   end IsAir32;
   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
      LEDs          : GPIO_Points := Red_LED & Green_LED & Blue_LED;
   begin
      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_2MHz;
      Configuration.Resistors   := Floating;
      Enable_Clock (LEDs);
      Configure_IO (LEDs,
                    Config => Configuration);
   end Initialize_LEDs;

   procedure All_LEDs_Off is
   begin
      Turn_Off (LCH_LED);
   end All_LEDs_Off;

   procedure Initialize_Board is
   begin
      if not IsAir32 then
         Configure_AF_SWD;
         --  ^^^ At PoR JTAG+SWD are ena. Reclaim JTAG pins
      end if;

      Initialize_LEDs;

   end Initialize_Board;

end STM32.Board;
