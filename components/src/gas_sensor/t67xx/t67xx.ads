pragma Restrictions (No_Streams);

with System;
--  with Interfaces; use Interfaces;
with HAL.I2C;    use HAL.I2C;

use HAL;

generic

   type IO_Port (<>) is abstract tagged limited private;

   type Any_IO_Port is access all IO_Port'Class;

   with procedure Read
     (This      : Any_IO_Port;
      Func      : UInt8;
      Register  : UInt16;
      Nxfer     : UInt16;
      Response  : out I2C_Data) is <>;

   --  Get the Value at the address specified in Register via This port.

   type Sensor_Data_Buffer is array (Natural range <>) of UInt8;

   with procedure Delay_Milliseconds (Count : Positive) is <>;
   --  Issue a relative delay for Count milliseconds. This formal subprogram
   --  removes the dependency on Ada.Real_Time so that this driver can be used
   --  with runtimes that do not have that language-defined facility.

package T67XX is

   type T67XX_Sensor (Port : not null Any_IO_Port) is tagged limited private;

   function Device_Id (This : in out T67XX_Sensor) return UInt16;

   function Co2_Reading (This : in out T67XX_Sensor) return UInt16;

   T67XX_Primary_Address   : constant I2C_Address := 16#15# * 2;
   --  shift left one bit since we're using 7-bit addresses.

private

   type T67XX_Sensor (Port : not null Any_IO_Port) is
   tagged limited record
      Mode : Natural;
   end record;

   FIRMWARE_REVISION      : constant := 16#1389#;
   STATUS                 : constant := 16#138A#;
   GAS_PPM                : constant := 16#138B#;
   RESET_DEVICE           : constant := 16#03E8#;
   START_SINGLE_POINT_CAL : constant := 16#03EC#;
   SLAVE_ADDRESS          : constant := 16#0FA5#;
   ABC_LOGIC              : constant := 16#03EE#;

end T67XX;
