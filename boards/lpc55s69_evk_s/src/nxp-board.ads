with NXP.Device;  use NXP.Device;
with NXP.GPIO;    use NXP.GPIO;
package NXP.Board is
   pragma Elaborate_Body;
   subtype User_LED is GPIO_Point;

   Red_LED      : User_LED renames P1_6;
   Green_LED    : User_LED renames P1_7;
   Blue_LED     : User_LED renames P1_4;

   LEDs         : GPIO_Points := Red_LED & Green_LED & Blue_LED;

   User_Button  : GPIO_Point renames P1_9;
   Wake_Button  : GPIO_Point renames P1_18;

   procedure Turn_Off (This : in out User_LED) renames NXP.GPIO.Set;
   procedure Turn_On (This : in out User_LED) renames NXP.GPIO.Clear;

   procedure Initialize_Board;
end NXP.Board;
