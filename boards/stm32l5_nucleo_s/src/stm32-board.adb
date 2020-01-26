with STM32.Setup;
with STM32_SVD.NVIC;  use STM32_SVD.NVIC;
with STM32_SVD.EXTI;  use STM32_SVD.EXTI;
--  vvvv see utils.adb in WB55...
--  private with Ada.Interrupts;
--  private with Ada.Interrupts.Names;

package body STM32.Board is

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (LEDs);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_2MHz;
      Configuration.Resistors   := Floating;
      Configuration.Secure      := False;
      Configure_IO (LEDs,
                    Config => Configuration);
   end Initialize_LEDs;

   procedure All_LEDs_Off is
   begin
      --      Turn_Off (LEDs);
      null;
   end All_LEDs_Off;

   procedure Initialize_Button is
      Configuration : GPIO_Port_Configuration;
      Index         : Integer;
   begin
         Enable_Clock (User_Button);
         Configuration.Mode        := Mode_In;
         Configuration.Output_Type := Push_Pull;
         Configuration.Resistors   := Floating;
         Configuration.Speed       := Speed_2MHz;
         Configuration.Secure      := False;

         Configure_IO (User_Button, Configuration);

         Index := GPIO_Pin'Pos (User_Button.Pin);
         Enable_External_Interrupt (External_Line_Number'Val (Index),
                                    Interrupt_Rising_Edge,
                                    False);
         --         NVIC_Periph.ITNS0 := NVIC_Periph.ITNS0 or (2 ** EXTI13_Interrupt);
         NVIC_Periph.ITNS0 := NVIC_Periph.ITNS0 or (2 ** 24);
         EXTI_Periph.EXTICR4.EXTI8_15 := 2; --  GPIOC[13]

   end Initialize_Button;

   procedure Initialize_Board is
   begin
--      Configure_AF_SWD;
      --  ^^^ At PoR JTAG+SWD are ena. Reclaim JTAG pins

      Initialize_LEDs;

      Initialize_Button;

   end Initialize_Board;

end STM32.Board;
