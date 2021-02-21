package body T67XX_I2C_IO is

   ----------
   -- Read --
   ----------

   procedure Read
     (This     : Any_IO_Port;
      Func     : UInt8;
      Register : UInt16;
      Nxfer    : UInt16;
      Response : out I2C_Data)
   is
      Result   : I2C_Status;
      Frame    : T67XX_Frame;
      Buf      : I2C_Data (1 .. 5);
      for Frame'Address use Buf'Address;
   begin
      Frame.Func   := Func;
      Frame.RegNum := Register;
      Frame.NReg   := Nxfer;
      Swap (Frame.RegNum);
      Swap (Frame.NReg);
      Master_Transmit
        (This.Port.all,
         This.Device,
         Buf,
         Result);
      if Result /= Ok then
         raise Program_Error with "I2C read pt1 error:" & Result'Img;
      end if;

      This.Ptr.all;
--      Delay_Milliseconds (10);

      Master_Receive (This.Port.all,
                      This.Device,
                      Response,
                      Result);
      if Result /= Ok then
         raise Program_Error with "I2C read pt2 error:" & Result'Img;
      end if;

   end Read;

   procedure Swap (X : in out UInt16)
   is
      Result : UInt16 := X;
   begin
      Result := Shift_Left (X, 8) or (Shift_Right (X, 8));
      X := Result;
   end Swap;

end T67XX_I2C_IO;
