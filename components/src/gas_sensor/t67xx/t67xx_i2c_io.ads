
with HAL.I2C;    use HAL.I2C;
use HAL;

package T67XX_I2C_IO is

   type Delay_Procedure_Type is access procedure;

   type IO_Port is tagged record
      Port   : Any_I2C_Port;
      Device : I2C_Address;
      Ptr    : Delay_Procedure_Type;
   end record;

   type Any_IO_Port is access all IO_Port'Class;

   procedure Read
     (This      : Any_IO_Port;
      Func      : UInt8;
      Register  : UInt16;
      Nxfer     : UInt16;
      Response  : out I2C_Data);

private
   type T67XX_Frame is record
      Func    : UInt8;
      RegNum  : UInt16;
      NReg    : UInt16;
   end record
     with Pack;

   procedure Swap (X : in out UInt16);

end T67XX_I2C_IO;
