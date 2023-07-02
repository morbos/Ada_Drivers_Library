with HAL;          use HAL;
package Logcmd is
   procedure Log (Cmd : UInt8);
   procedure Log1Byte (Cmd : UInt8; Data : UInt8);
   procedure Log2Byte (Cmd : UInt8; Hi : UInt8; Lo : UInt8);
end Logcmd;
