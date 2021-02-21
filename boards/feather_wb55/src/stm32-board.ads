with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;

package STM32.Board is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Point;

   Red_LED   : User_LED renames PB2;
   LCH_LED   : User_LED renames PB2;

   procedure Initialize_Board;
   procedure Initialize_LEDs;

   procedure Turn_On  (This : in out User_LED) renames STM32.GPIO.Set;
   procedure Turn_Off (This : in out User_LED) renames STM32.GPIO.Clear;

   procedure All_LEDs_Off;

end STM32.Board;
