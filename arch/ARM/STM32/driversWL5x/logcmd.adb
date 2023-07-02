package body Logcmd is

   type LogIdx_T is mod 8192;

   type Logbuf_T is array (LogIdx_T) of UInt8;

   Logbuf : Logbuf_T;

   LogIdx : LogIdx_T := 0;

   procedure Log (Cmd : UInt8) is
   begin
      Logbuf (LogIdx) := Cmd;
      LogIdx := LogIdx + 1;
   end Log;

   procedure Log1Byte (Cmd : UInt8; Data : UInt8) is
   begin
      Log (Cmd);
      Log (Data);
   end Log1Byte;

   procedure Log2Byte (Cmd : UInt8; Hi : UInt8; Lo : UInt8) is
   begin
      Log (Cmd);
      Log (Hi);
      Log (Lo);
   end Log2Byte;

end Logcmd;
