
package body T67XX is

   ---------------
   -- Device_Id --
   ---------------
   function Device_Id (This : in out T67XX_Sensor) return UInt16
   is
      Answer : I2C_Data (1 .. 4);
   begin
      Read (This.Port, 4, FIRMWARE_REVISION, 1, Answer);
      return Shift_Left (UInt16 (Answer (3)), 8) or UInt16 (Answer (4));
   end Device_Id;

   -----------------
   -- Co2_Reading --
   -----------------
   function Co2_Reading (This : in out T67XX_Sensor) return UInt16
   is
      Answer : I2C_Data (1 .. 4);
   begin
      Read (This.Port, 4, GAS_PPM, 1, Answer);
      return Shift_Left (UInt16 (Answer (3)), 8) or UInt16 (Answer (4));
   end Co2_Reading;

end T67XX;
